%{
  open Mc

  type declaration =
  | Global of typ * string * expr
  | Fonction of fun_def
%}

%type <Mc.expr> expr

%token <int> CONST
%token <string> ID
%token PAR_O PAR_F BR_O BR_F
%token PLUS FOIS LT EQUAL
%token PUTCHAR IF ELSE WHILE RETURN FOR
%token VOID INT BOOL
%token COMMA SEMI
%token EOF

%type <Mc.prog> prog

%nonassoc LT
%left PLUS
%left FOIS

%start prog
%%

prog:
| d=list(declaration) EOF
  {
    let globals, fonctions =
      List.fold_right
        (fun d (g, f) ->
          match d with
          | Global(t, n, e) -> ((t, n, e)::g, f)
          | Fonction(fn) -> (g, fn::f))
        d
        ([], [])
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
    let globals, sets =
      List.fold_right
        (fun (t, n, e) (g, s) -> ((n, t)::g, (Set(n, e))::s))
        globals
        ([], [])
    in
    let main =
      {
        name = main.name;
        params = main.params;
        return = main.return;
        locals = main.locals;
        code = sets @ main.code;
      }
    in
    {
      globals = globals;
      functions = main::others;
    }
  }

declaration:
| v=var SEMI
  {
    let (t, n, e) = v in
    Global(t, n, e)
  }
| f=fonction
  {
    Fonction(f)
  }

fonction:
| t=typ n=ID PAR_O p=separated_list(COMMA, param_def) PAR_F BR_O s=seq BR_F
  {
    let (code, locals) = List.split s in
    {
      name = n;
      params = List.map (fun (t, n) -> (n, t)) p;
      return = t;
      locals = List.concat locals;
      code = code;
    }
  }

param_def:
| t=typ n=ID
  {
    (t, n)
  }

seq: 
| s=list(instr)
  {
    s
  }

var:
| t=typ n=ID EQUAL e=expr 
  {
    (t, n, e)
  }

instr:
| PUTCHAR PAR_O e=expr PAR_F SEMI
  {
    (Putchar(e), [])
  }
| v=var SEMI
  {
    let (t, n, e) = v in
    (Set(n, e), [(n, t)])
  }
| n=ID EQUAL e=expr SEMI
  {
    (Set(n, e), [])
  }
| IF PAR_O e=expr PAR_F BR_O s1=seq BR_F ELSE BR_O s2=seq BR_F
  {
    let s1, l1 = List.split s1 in
    let s2, l2 = List.split s2 in
    (If(e, s1, s2), (List.concat l1)@(List.concat l2))
  }
| WHILE PAR_O e=expr PAR_F BR_O s=seq BR_F
  {
    let s, l = List.split s in
    (While(e, s), List.concat l)
  }
| FOR PAR_O v=var SEMI e=expr SEMI i=instr PAR_F BR_O s=seq BR_F
  {
    let (t, n, _) = v in
    let s, sl = List.split s in
    let i, il = i in
    (While(e, s@[i]), (n, t)::(il @ (List.concat sl)))
  }
| RETURN e=expr SEMI
  {
    (Return(e), [])
  }
| e=expr SEMI
  {
    (Expr(e), [])
  }

expr:
| c=CONST
  {
    Cst(c)
  }
| e1=expr PLUS e2=expr
  {
    Add(e1, e2)
  }
| e1=expr FOIS e2=expr
  {
    Mul(e1, e2)
  }
| e1=expr LT e2=expr
  {
    Lt(e1, e2)
  }
| n=ID
  {
    Get(n)
  }
| n=ID PAR_O p=separated_list(COMMA, expr) PAR_F
  {
    Call(n, p)
  }
| PAR_O e=expr PAR_F
  {
    e
  }

typ:
| VOID
  {
    Void
  }
| INT
  {
    Int
  }
| BOOL
  {
    Bool
  }