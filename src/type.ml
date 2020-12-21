open Mc

type fun_sig = {
  params: typ list;
  return: typ;
}

type typ_env = {
  globals:   (string * typ) list;
  locals:    (string * typ) list;
  functions: (string * fun_sig) list;
}

let rec base_typ_env (prog: prog) =
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

and type_var env n =
  try 
    List.assoc n env.locals
  with Not_found -> 
  try
    List.assoc n env.globals
  with Not_found -> failwith ("variable " ^ n ^ " : does not exist")

and type_unary_op env op e =
  match op with
  | Minus ->
    if type_expr env e = Int 
    then Int
    else failwith "can't take the oposite of non int value"
  | Not ->
    if type_expr env e = Bool 
    then Bool
    else failwith "can't inverse non bool value"

and type_binary_op env op e1 e2 =
  match op with 
  | Add ->
    if type_expr env e1 = Int && type_expr env e2 = Int
    then Int
    else failwith "can't add non int value"
  | Sub ->
    if type_expr env e1 = Int && type_expr env e2 = Int
    then Int
    else failwith "can't sub non int value"
  | Mul -> 
    if type_expr env e1 = Int && type_expr env e2 = Int
    then Int
    else failwith "can't mul non int value"
  | Div -> if type_expr env e1 = Int && type_expr env e2 = Int
    then Int
    else failwith "can't divide non int value"
  | Mod -> if type_expr env e1 = Int && type_expr env e2 = Int
    then Int
    else failwith "can't mod non int value"
  | Lt -> 
    if type_expr env e1 = Int && type_expr env e2 = Int
    then Bool
    else failwith "can't compare non int value"
  | And ->
    if type_expr env e1 = Bool && type_expr env e2 = Bool
    then Bool
    else failwith "can't and non bool value"
  | Or ->
    if type_expr env e1 = Bool && type_expr env e2 = Bool
    then Bool
    else failwith "can't or non bool value"
  | Eq ->
    if type_expr env e1 = type_expr env e2
    then Bool
    else failwith "both part of an equality must have the same type"


and type_expr (env : typ_env) (expr : expr) =
  match expr with
  | Cst(_) -> Int

  | Unary(op, e) -> type_unary_op env op e

  | Binary(op, e1, e2) -> type_binary_op env op e1 e2

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
  | Putchar(e) -> 
    if type_expr env e = Int
    then true
    else failwith "putchar can only be applied to int value"

  | Set(n, e) -> 
    if type_var env n = type_expr env e
    then true
    else failwith "variable and expression must have the same type in assigement"

  | If(b, s1, s2) ->
    if type_expr env b = Bool
    then 
      well_typed_sequence env rt s1 &&
      well_typed_sequence env rt s2
    else failwith "if statement can only be applied to boolean"

  | While(b, s) ->
    if type_expr env b = Bool
    then 
      well_typed_sequence env rt s
    else failwith "while statement can only be applied to boolean"

  | Return(e) -> 
    if type_expr env e = rt
    then true
    else failwith "return value type and fonction return type must be the same"

  | Expr(e) -> let _  = type_expr env e in true

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

and well_typed (prog : prog) =
  let env = base_typ_env prog in
  List.for_all (well_typed_function env) prog.functions