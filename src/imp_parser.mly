%type <Imp.expr> expr

%token <int> CONST
%token <string> ID
%token PAR_O PAR_F BR_O BR_F
%token PLUS FOIS LT EQUAL
%token PUTCHAR IF ELSE WHILE RETURN
%token VOID INT BOOL
%token COMMA SEMI

%token EOF

%type <(string * Imp.typ)> var
%type <Imp.instr> instr
%type <Imp.seq> seq
%type <Imp.typ> typ
%type <Imp.fun_def> fonction
%type <Imp.prog> prog

%start prog
%%

prog:
| list(var) nonempty_list(fonction) EOF 
  {
    {
      globals = [];
      functions = []
    }
  }

var:
| typ ID EQUAL expr SEMI
  {}

fonction:
| typ ID PAR_O separated_list(COMMA, param_def) PAR_F BR_O seq BR_F
  {}

param_def:
| typ ID
  {}

seq: 
| list(instr)
  {}

instr:
| PUTCHAR PAR_O expr PAR_F SEMI
  {}
| var
  {}
| IF PAR_O expr PAR_F BR_O seq BR_F ELSE BR_O seq BR_F
  {}
| WHILE PAR_O expr PAR_F BR_O seq BR_F
  {}
| RETURN expr SEMI
  {}
| expr SEMI
  {}

expr:
| CONST
  {}
| expr PLUS expr
  {}
| expr FOIS expr
  {}
| expr LT expr
  {}
| ID
  {}
| ID PAR_O separated_list(COMMA, expr) PAR_F
  {}

typ:
| VOID
  {}
| INT
  {}
| BOOL
  {}