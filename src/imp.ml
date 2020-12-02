(**
   Syntaxe abstraite IMP
*)

type expr =
  (* Arithmétique *)
  | Cst of int
  | Add of expr * expr
  | Mul of expr * expr
  | Lt  of expr * expr
  (* Variables *)
  | Get of string
  (* Fonctions *)
  | Call of string * expr list

type instr =
  | Putchar of expr
  (* Variables *)
  | Set of string * expr
  (* Contrôle *)
  | If  of expr * seq * seq
  | While of expr * seq
  (* Fonctions *)
  | Return of expr
  | Expr of expr
and seq = instr list

type function_def = {
  name: string;
  params: string list;
  locals: string list;
  code: seq;
}

type prog = {
  globals: string list;
  functions: function_def list;
}
