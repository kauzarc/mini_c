(**
   Syntaxe abstraite IMP
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

type fun_sig = {
  params: typ list;
  return: typ;
}

type typ_env = {
  globals:   (string * typ) list;
  locals:    (string * typ) list;
  functions: (string * fun_sig) list;
}

let base_typ_env (prog: prog) =
  let globals = prog.globals in
  let functions =
    List.map 
      (fun (f : fun_def) -> 
         (f.name, {params = List.map (fun (n, t) -> t) f.params; return = f.return}))
      prog.functions
  in
  {
    globals = globals;
    locals = [];
    functions = functions
  }

let type_var env n =
  try 
    List.assoc n env.locals
  with Not_found -> 
  try
    List.assoc n env.globals
  with Not_found -> failwith ("variable " ^ n ^ " : does not exist")

let rec type_expr (env : typ_env) (expr : expr) =
  match expr with
  | Cst(_) -> Int

  | Add(e1, e2) ->
    if type_expr env e1 = Int && type_expr env e2 = Int
    then Int
    else failwith "can't add non int value"

  | Mul(e1, e2) ->
    if type_expr env e1 = Int && type_expr env e2 = Int
    then Int
    else failwith "can't mul non int value"

  | Lt(e1, e2) ->
    if type_expr env e1 = Int && type_expr env e2 = Int
    then Bool
    else failwith "can't compare non int value"

  | Get(n) -> type_var env n

  | Call(n, el) ->
    let fun_sig = try
        List.assoc n env.functions
      with Not_found -> failwith ("function " ^ n ^ " : does not exist")
    in
    if 
      try
        List.for_all2 (fun e t -> type_expr env e = t) el fun_sig.params
      with Invalid_argument(_) -> failwith ("function " ^ n ^ " : wrong number of argument")
    then fun_sig.return
    else failwith ("function " ^ n ^ " : wrong type of argument")

and well_typed_instruction (env : typ_env) (rt : typ) (i : instr) =
  match i with
  | Putchar(e) -> type_expr env e = Int

  | Set(n, e) -> type_var env n = type_expr env e

  | If(b, s1, s2) -> 
    type_expr env b = Bool && 
    well_typed_sequence env rt s1 &&
    well_typed_sequence env rt s2

  | While(b, s) ->
    type_expr env b = Bool && 
    well_typed_sequence env rt s

  | Return(e) -> type_expr env e = rt

  | Expr(e) -> true

and well_typed_sequence (env : typ_env) (rt : typ) (s : seq) = 
  List.for_all (well_typed_instruction env rt) s

and well_typed_function (env : typ_env) (f : fun_def) =
  let env = {
    globals = env.globals;
    locals = f.locals @ f.params;
    functions = env.functions
  }
  in
  well_typed_sequence env f.return f.code

and well_typed_prog (prog : prog) =
  let env = base_typ_env prog in
  List.for_all (well_typed_function env) prog.functions