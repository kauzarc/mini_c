%{
  open Mc

  type declaration =
    | Globals of string * typ * (expr option)
    | Fonction of fun_def
%}

%token <int> CONST
%token <string> ID
%token EQUAL
%token PAR_O PAR_F BR_O BR_F
%token PLUS MINUS FOIS SLASH LT MT DBLEQUAL EXCM AND OR
%token PUTCHAR IF ELSE WHILE RETURN FOR
%token VOID INT BOOL
%token COMMA SEMI
%token EOF

%type <Mc.prog> prog

%nonassoc DBLEQUAL
%nonassoc EXCM
%left AND OR
%nonassoc LT MT
%left PLUS MINUS
%left FOIS SLASH

%start prog
%%

prog:
| d=list(global_declaration) EOF 
  {
    let globals, fonctions, defaults =
      List.fold_right
        (fun dec (g, f, def) ->
          match dec with
          | Globals(n, t, Some(e)) -> ((n, t)::g, f, (Set(n, e))::def)
          | Globals(n, t, None) -> ((n, t)::g, f, def)
          | Fonction(fonc) -> (g, fonc::f, def))
        d
        ([], [], [])
    in
    let mains, others =
      List.partition
        (fun f -> f.name = "main")
        fonctions
    in
    let main =
      match mains with
      | [] -> failwith "can't find main fonction"
      | main::[] -> main
      | _ -> failwith "can't declare more than one main function"
    in
    let main =
      {
        name = main.name;
        params = main.params;
        return = main.return;
        locals = main.locals;
        code = defaults @ main.code;
      }
    in
    {
      globals = globals;
      functions = main::others;
    }
  }

global_declaration:
| v=var SEMI { let n, t, e = v in Globals(n, t, e) }
| f=fonction { Fonction(f) }

fonction:
| t=typ n=ID PAR_O p=separated_list(COMMA, param_def) PAR_F BR_O s=seq BR_F 
  {
    let s, v = s in
    {
      name = n;
      params = p;
      return = t;
      locals = v;
      code = s;
    }
  }

param_def:
| t=typ n=ID { (n, t) }

seq: 
| s=list(instr) { let i, v = List.split s in (List.concat i, List.concat v)  } 

var:
| t=typ n=ID EQUAL e=expr { (n, t, Some(e)) }
| t=typ n=ID { (n, t, None) }

instr:
| i=simple_instr SEMI { i }
| b=branching { b }

simple_instr:
| v=var { match v with (n, t, Some(e)) -> ([Set(n, e)], [(n, t)]) | (n, t, None) -> ([], [(n, t)]) }
| PUTCHAR PAR_O e=expr PAR_F { ([Putchar(e)], []) }
| n=ID EQUAL e=expr { ([Set(n, e)], []) }
| RETURN e=expr { ([Return(e)], []) }
| e=expr { ([Expr(e)], []) }

branching:
| IF PAR_O e=expr PAR_F BR_O s=seq BR_F { let s, v = s in ([If(e, s, [])], v) }
| IF PAR_O e=expr PAR_F BR_O s1=seq BR_F ELSE BR_O s2=seq BR_F { let s1, v1 = s1 in let s2, v2 = s2 in ([If(e, s1, s2)], v1 @ v2) }
| WHILE PAR_O e=expr PAR_F BR_O s=seq BR_F { let s, v = s in ([While(e, s)], v) }
| FOR PAR_O before=simple_instr SEMI e=expr SEMI after=simple_instr PAR_F BR_O s=seq BR_F 
  {
    let befores, beforev = before in
    let afters, afterv = after in
    let s, v = s in
    (befores @ [While(e, s @ afters)], beforev @ v @ afterv)
  }

expr:
| n=CONST { Cst(n) }
| n=ID { Get(n) }
| n=ID PAR_O p=separated_list(COMMA, expr) PAR_F { Call(n, p) }
| PAR_O e=expr PAR_F { e }
| e=unary_op { e }
| e=binary_op { e }


unary_op:
| MINUS e=expr { Unary(Minus, e) }
| EXCM e=expr { Unary(Not, e) }

binary_op:
| e1=expr PLUS e2=expr { Binary(Add, e1, e2) }
| e1=expr MINUS e2=expr { Binary(Sub, e1, e2) }
| e1=expr FOIS e2=expr { Binary(Mul, e1, e2) }
| e1=expr SLASH e2=expr { Binary(Div, e1, e2) }
| e1=expr LT e2=expr { Binary(Lt, e1, e2) }
| e1=expr MT e2=expr { Binary(Lt, e2, e1) }
| e1=expr DBLEQUAL e2=expr { Binary(Eq, e1, e2) }
| e1=expr AND AND e2=expr { Binary(And, e1, e2) }
| e1=expr OR OR e2=expr { Binary(Or, e1, e2) }

typ:
| VOID { Void }
| INT { Int }
| BOOL { Bool }