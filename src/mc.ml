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

let print prog =
  let rec print_prog (p : prog) =
    Printf.printf "----------GLOBALS:----------\n";
    List.iter print_var p.globals;
    Printf.printf "\n----------FONCTION:----------\n";
    List.iter print_fn p.functions

  and print_var (n, t) = 
    Printf.printf "var %s : %s\n" n (typ_to_string t)

  and print_fn (fn : fun_def) =
    Printf.printf "\n- fonction %s:\n" fn.name;
    Printf.printf "return %s:\n" (typ_to_string fn.return);
    Printf.printf "params:\n";
    List.iter print_var fn.params;
    Printf.printf "locals:\n";
    List.iter print_var fn.locals;
    Printf.printf "code:\n";
    List.iter print_instr fn.code

  and print_instr instr =
    match instr with
    | Putchar(e) -> Printf.printf "Putchar(%s);\n" (expr_to_string e)
    | Set(s, e) -> Printf.printf "Set(%s, %s);\n" s (expr_to_string e)
    | If(e, s1, s2) ->
      Printf.printf "If(%s,\n[" (expr_to_string e);
      List.iter print_instr s1;
      Printf.printf "], \n[";
      List.iter print_instr s2;
      Printf.printf "])\n"
    | While(e, s) ->
      Printf.printf "While(%s,\n[" (expr_to_string e);
      List.iter print_instr s;
      Printf.printf "])\n";
    | Return(e) -> Printf.printf "Return(%s)\n" (expr_to_string e) 
    | Expr(e) -> Printf.printf "Expr(%s)\n" (expr_to_string e) 

  and expr_to_string expr =
    match expr with
    | Cst(n) -> "Cst(" ^ (string_of_int n) ^ ")"
    | Add(e1, e2) -> "Add(" ^ (expr_to_string e1) ^ ", " ^ (expr_to_string e2) ^ ")"
    | Mul(e1, e2) -> "Mul(" ^ (expr_to_string e1) ^ ", " ^ (expr_to_string e2) ^ ")"
    | Lt(e1, e2) -> "Lt(" ^ (expr_to_string e1) ^ ", " ^ (expr_to_string e2) ^ ")"
    | Get(s) -> "Get(" ^ s ^ ")"
    | Call(s, l) -> "Call(" ^ s ^ ", [" ^ (List.fold_left (fun acc e -> acc ^ (expr_to_string e) ^ ", ") "" l) ^ "])"

  and typ_to_string t =
    match t with
    | Void -> "void"
    | Int -> "int"
    | Bool -> "bool"
  in
  print_prog prog


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


and type_expr (env : typ_env) (expr : expr) =
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

and well_typed (prog : prog) =
  let env = base_typ_env prog in
  List.for_all (well_typed_function env) prog.functions