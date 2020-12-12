(**
   Syntaxe abstraite Mini-C
*)

type expr =
  (* Arithmétique *)
  | Cst  of int
  | Add  of expr * expr
  | Mul  of expr * expr
  | Lt   of expr * expr
  (* Variables *)
  | Get  of string
  (* Fonctions *)
  | Call of string * expr list

type instr =
  | Putchar of expr
  (* Variables *)
  | Set     of string * expr
  (* Contrôle *)
  | If      of expr * seq * seq
  | While   of expr * seq
  (* Fonctions *)
  | Return  of expr
  | Expr    of expr
and seq = instr list

type typ =
  | Int
  | Bool
  | Void

type fun_def = {
  name:   string;
  params: (string * typ) list;
  return: typ;
  locals: (string * typ) list;
  code:   seq;
}

type prog = {
  globals:   (string * typ) list;
  functions: fun_def list;
}

let example = {
  globals=[
    ("PARAM",Int)
  ];
  functions=[
    {
      name="main";
      params=[];
      return=Void;
      locals=[];
      code=[
        Set("PARAM",Cst(5));
        Putchar(Call("fact",[Get("PARAM")]))
      ]
    };
    {
      name="fact";
      params=[("n",Int)];
      return=Int;
      locals=[];
      code=[
        If(
          Lt(Get("n"),Cst(2)),
          [Return(Cst(1))],
          [Return(Mul(Get("n"),Call("fact",[Add(Get("n"),Cst(-1))])))])
      ]
    }
  ]
}

let a_list_to_string a_to_string l =
  let rec aux l acc = 
    match l with
    | [] -> acc
    | a::[] ->  acc ^ (a_to_string a)
    | a::lt -> aux lt (acc ^ a_to_string a ^ ";")
  in
  (aux l "[") ^ "]"

let rec prog_to_string prog =
  "{globals=" ^ (a_list_to_string var_to_string prog.globals) ^ ";" ^
  "functions=" ^ (a_list_to_string fun_def_to_string prog.functions) ^ "}"

and var_to_string var =
  "(\"" ^ (fst var) ^ "\"," ^ (typ_to_string (snd var)) ^ ")"

and typ_to_string typ =
  match typ with
  | Void -> "Void"
  | Int -> "Int"
  | Bool -> "Bool"

and fun_def_to_string fun_def =
  "{name=\"" ^ fun_def.name ^ "\";" ^
  "params=" ^ (a_list_to_string var_to_string fun_def.params) ^ ";" ^
  "return=" ^ (typ_to_string fun_def.return) ^ ";" ^
  "locals=" ^ (a_list_to_string var_to_string fun_def.locals) ^ ";" ^
  "code=" ^ (a_list_to_string instr_to_string fun_def.code) ^ "}"

and instr_to_string instr =
  match instr with
  | Putchar(e) -> "Putchar(" ^ (expr_to_string e) ^ ")"
  | Set(name, e) -> "Set(\"" ^ name ^ "\"," ^ (expr_to_string e) ^ ")"
  | If(e, s1, s2) -> "If(" ^ (expr_to_string e) ^ "," ^ (a_list_to_string instr_to_string s1) ^ "," ^ (a_list_to_string instr_to_string s2) ^ ")"
  | While(e, s) ->"While(" ^ (expr_to_string e) ^ "," ^ (a_list_to_string instr_to_string s) ^ ")"
  | Return(e) -> "Return(" ^ (expr_to_string e) ^ ")"
  | Expr(e) -> "Expr(" ^ (expr_to_string e) ^ ")"

and expr_to_string expr =
  match expr with
  | Cst(n) -> "Cst(" ^ (string_of_int n) ^ ")"
  | Add(e1, e2) -> "Add(" ^ (expr_to_string e1) ^ "," ^ (expr_to_string e2) ^ ")"
  | Mul(e1, e2) -> "Mul(" ^ (expr_to_string e1) ^ "," ^ (expr_to_string e2) ^ ")"
  | Lt(e1, e2) -> "Lt(" ^ (expr_to_string e1) ^ "," ^ (expr_to_string e2) ^ ")"
  | Get(name) -> "Get(\"" ^ name ^ "\")"
  | Call(name, l) -> "Call(\"" ^ name ^ "\"," ^ (a_list_to_string expr_to_string l) ^ ")"