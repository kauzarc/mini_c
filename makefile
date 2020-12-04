EXEC= app

SRC_PATH= src
BUILD_PATH= build

CAML_FILE= $(wildcard $(SRC_PATH)/*.ml)
LEX_FILE= $(wildcard $(SRC_PATH)/*.mll)
YACC_FILE= $(wildcard $(SRC_PATH)/*.mly)

CAML_OBJ= $(CAML_FILE:$(SRC_PATH)/%.ml=$(BUILD_PATH)/%.cmx)
LEX_OBJ= $(LEX_FILE:$(SRC_PATH)/%.mll=$(BUILD_PATH)/%.cmx)
YACC_OBJ= $(YACC_FILE:$(SRC_PATH)/%.mly=$(BUILD_PATH)/%.cmx)

OBJ= $(CAML_OBJ) $(YACC_OBJ) $(LEX_OBJ) 

all: prep $(EXEC)

$(EXEC): $(OBJ)
	ocamlopt $^ -o $@

$(BUILD_PATH)/%.cmx:  $(SRC_PATH)/%.ml 
	ocamlopt -c $< -o $@ -I $(BUILD_PATH)

$(BUILD_PATH)/%.cmx:  $(BUILD_PATH)/%.ml 
	ocamlopt -c $< -o $@ -I $(BUILD_PATH)

$(BUILD_PATH)/%.ml: $(SRC_PATH)/%.mll
	ocamllex -o $@ $<

$(BUILD_PATH)/%.ml: $(SRC_PATH)/%.mly
	menhir --base $(@:$(BUILD_PATH)/%.ml=$(BUILD_PATH)/%) $<
	ocamlc -o $(@:$(BUILD_PATH)/%.ml=$(BUILD_PATH)/%.cmi) $(@:$(BUILD_PATH)/%.ml=$(BUILD_PATH)/%.mli) -I $(BUILD_PATH)

prep:
	mkdir -p $(BUILD_PATH)

clean:
	rm -rf $(BUILD_PATH)/* $(EXEC)