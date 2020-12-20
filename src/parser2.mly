%{
  open Mc
%}

%token <int> CONST
%token <string> ID
%token PAR_O PAR_F BR_O BR_F
%token PLUS FOIS LT EQUAL
%token PUTCHAR IF ELSE WHILE RETURN FOR
%token VOID INT BOOL
%token COMMA SEMI
%token EOF

%type <unit> prog

%nonassoc LT
%left PLUS
%left FOIS

%start prog
%%

prog:
| list(global_declaration) EOF {}

global_declaration:
| var SEMI {}
| fonction {}

fonction:
| typ ID PAR_O separated_list(COMMA, param_def) PAR_F BR_O seq BR_F {}

param_def:
| typ ID {}

seq: 
| list(instr) {}

var:
| typ ID EQUAL expr {}
| typ ID {}

instr:
| semi_instr SEMI {}
| non_semi_instr {}

semi_instr:
| PUTCHAR PAR_O expr PAR_F {}
| var {}
| ID EQUAL expr {}
| RETURN expr {}
| expr {}

non_semi_instr:
| IF PAR_O expr PAR_F BR_O seq BR_F {}
| IF PAR_O expr PAR_F BR_O seq BR_F ELSE BR_O seq BR_F {}
| WHILE PAR_O expr PAR_F BR_O seq BR_F {}
| FOR PAR_O var SEMI expr SEMI instr PAR_F BR_O seq BR_F {}

expr:
| CONST {}
| expr PLUS expr {}
| expr FOIS expr {}
| expr LT expr {}
| ID {}
| ID PAR_O p=separated_list(COMMA, expr) PAR_F {}
| PAR_O expr PAR_F {}

typ:
| VOID {}
| INT {}
| BOOL {}