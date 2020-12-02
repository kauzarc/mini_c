%token ID LPAR RPAR COMMA EOF
%start prog
%type <unit> prog
%%

prog:
| expr EOF {}

expr:
| simple_expr {}
| ID simple_expr_list {}
| ID tuple {}

simple_expr:
| ID {}
| LPAR expr RPAR {}
| tuple {}

tuple:
| LPAR RPAR {}
| LPAR expr_comma_separated_list RPAR {}

simple_expr_list:
| simple_expr {}
| simple_expr_list simple_expr {}

expr_comma_separated_list:
| expr {}
| expr_comma_separated_list COMMA expr {}