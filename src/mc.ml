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

let string_of_a_list string_of_a l =
  let rec aux l acc = 
    match l with
    | [] -> acc
    | a::[] ->  acc ^ (string_of_a a)
    | a::lt -> aux lt (acc ^ string_of_a a ^ ";")
  in
  (aux l "[") ^ "]"

let rec string_of_prog prog =
  "{globals=" ^ (string_of_a_list string_of_var prog.globals) ^ ";" ^
  "functions=" ^ (string_of_a_list string_of_fun_def prog.functions) ^ "}"

and string_of_var var =
  "(\"" ^ (fst var) ^ "\"," ^ (string_of_typ (snd var)) ^ ")"

and string_of_typ typ =
  match typ with
  | Void -> "Void"
  | Int -> "Int"
  | Bool -> "Bool"

and string_of_fun_def fun_def =
  "{name=\"" ^ fun_def.name ^ "\";" ^
  "params=" ^ (string_of_a_list string_of_var fun_def.params) ^ ";" ^
  "return=" ^ (string_of_typ fun_def.return) ^ ";" ^
  "locals=" ^ (string_of_a_list string_of_var fun_def.locals) ^ ";" ^
  "code=" ^ (string_of_a_list string_of_instr fun_def.code) ^ "}"

and string_of_instr instr =
  match instr with
  | Putchar(e) -> "Putchar(" ^ (string_of_expr e) ^ ")"
  | Set(name, e) -> "Set(\"" ^ name ^ "\"," ^ (string_of_expr e) ^ ")"
  | If(e, s1, s2) -> "If(" ^ (string_of_expr e) ^ "," ^ (string_of_a_list string_of_instr s1) ^ "," ^ (string_of_a_list string_of_instr s2) ^ ")"
  | While(e, s) ->"While(" ^ (string_of_expr e) ^ "," ^ (string_of_a_list string_of_instr s) ^ ")"
  | Return(e) -> "Return(" ^ (string_of_expr e) ^ ")"
  | Expr(e) -> "Expr(" ^ (string_of_expr e) ^ ")"

and string_of_expr expr =
  match expr with
  | Cst(n) -> "Cst(" ^ (string_of_int n) ^ ")"
  | Add(e1, e2) -> "Add(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Mul(e1, e2) -> "Mul(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Lt(e1, e2) -> "Lt(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Get(name) -> "Get(\"" ^ name ^ "\")"
  | Call(name, l) -> "Call(\"" ^ name ^ "\"," ^ (string_of_a_list string_of_expr l) ^ ")"