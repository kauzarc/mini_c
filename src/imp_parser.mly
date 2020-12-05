%{
  open Imp
%}

%type <Imp.expr> expr

%token <int> CONST
%token <string> ID
%token PAR_O PAR_F BR_O BR_F
%token PLUS FOIS LT EQUAL
%token PUTCHAR IF ELSE WHILE RETURN
%token VOID INT BOOL
%token COMMA SEMI
%token EOF

%type <Imp.instr> instr
%type <Imp.seq> seq
%type <Imp.typ> typ
%type <Imp.fun_def> fonction
%type <Imp.prog> prog

%left PLUS
%left FOIS
%nonassoc LT

%start prog
%%

prog:
| gl=list(var) fn=nonempty_list(fonction) EOF 
  {
    let (var, set) =
      List.split 
        (List.map 
          (fun (n, t, e) -> ((n, t), (n, e)))
          gl
        ) 
    in
    {
      globals = var;
      functions = fn
    }
  }

var:
| t=typ n=ID EQUAL e=expr SEMI 
  {
    (n, t, e)
  }

fonction:
| t=typ n=ID PAR_O p=separated_list(COMMA, param_def) PAR_F BR_O v=list(var) s=seq BR_F
  {
    let (var, set) = 
      List.split
        (List.map 
          (fun (n, t, e) -> ((n, t), Set(n, e)))
          v
        )
    in
    {
      name = n;
      params = p;
      return = t;
      locals = var;
      code = set @ s
    }
  }

param_def:
| t=typ n=ID
  {
    (n, t)
  }

seq: 
| s=list(instr)
  {
    s
  }

instr:
| PUTCHAR PAR_O e=expr PAR_F SEMI
  {
    Putchar(e)
  }
| n=ID EQUAL e=expr SEMI
  {
    Set(n, e)
  }
| IF PAR_O e=expr PAR_F BR_O s1=seq BR_F ELSE BR_O s2=seq BR_F
  {
    If(e, s1, s2)
  }
| WHILE PAR_O e=expr PAR_F BR_O s=seq BR_F
  {
    While(e, s)
  }
| RETURN e=expr SEMI
  {
    Return(e)
  }
| e=expr SEMI
  {
    Expr(e)
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