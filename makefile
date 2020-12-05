EXEC= app

SRC_PATH= src
BUILD_PATH= build

CAML_FILE= $(wildcard $(SRC_PATH)/*.ml)
CAMLI_FILE= $(wildcard $(SRC_PATH)/*.mli)
LEX_FILE= $(wildcard $(SRC_PATH)/*.mll)
YACC_FILE= $(wildcard $(SRC_PATH)/*.mly)

CAML_OBJ= $(CAML_FILE:$(SRC_PATH)/%.ml=$(BUILD_PATH)/%.cmx)
CAMLI_OBJ= $(CAMLI_FILE:$(SRC_PATH)/%.mli=$(BUILD_PATH)/%.cmi)
LEX_OBJ= $(LEX_FILE:$(SRC_PATH)/%.mll=$(BUILD_PATH)/%.cmx)
YACC_OBJ= $(YACC_FILE:$(SRC_PATH)/%.mly=$(BUILD_PATH)/%.cmx)

CONFLICTS= $(YACC_FILE:$(SRC_PATH)/%.mly=$(BUILD_PATH)/%.conflicts)

OBJ= $(YACC_OBJ) $(LEX_OBJ) $(CAML_OBJ)

all: prep $(EXEC)

$(EXEC): $(CAMLI_OBJ) $(OBJ)
	ocamlopt $(OBJ) -o $@

$(BUILD_PATH)/%.cmi: $(SRC_PATH)/%.mli
	ocamlc -c $< -o $@ -I $(BUILD_PATH)

$(BUILD_PATH)/%.cmx: $(SRC_PATH)/%.ml 
	ocamlopt -c $< -o $@ -I $(BUILD_PATH)

$(BUILD_PATH)/%.cmx: $(BUILD_PATH)/%.ml 
	ocamlopt -c $< -o $@ -I $(BUILD_PATH)

$(BUILD_PATH)/%.ml: $(SRC_PATH)/%.mll
	ocamllex -o $@ $<

$(BUILD_PATH)/%.ml: $(SRC_PATH)/%.mly
	menhir --base $(@:$(BUILD_PATH)/%.ml=$(BUILD_PATH)/%) $<
	ocamlc -o $(@:$(BUILD_PATH)/%.ml=$(BUILD_PATH)/%.cmi) -c $(@:$(BUILD_PATH)/%.ml=$(BUILD_PATH)/%.mli) -I $(BUILD_PATH)

prep:
	mkdir -p $(BUILD_PATH)

clean:
	rm -rf $(BUILD_PATH)/* $(EXEC)

menhir_v: $(CAMLI_OBJ) $(CONFLICTS)

$(BUILD_PATH)/%.conflicts: $(SRC_PATH)/%.mly
	menhir -v --base $(@:$(BUILD_PATH)/%.conflicts=$(BUILD_PATH)/%) $<
