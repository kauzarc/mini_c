open Mc

type value =
  | Int of int
  | Bool of bool
  | Undefined

module Env =
struct
  type t = {
    fonctions: (string, fun_def) Hashtbl.t;
    globals: (string, value) Hashtbl.t;
    locals: (string, value) Hashtbl.t;
    params: (string, value) Hashtbl.t;
  }

  let get_var env name =
    try 
      Hashtbl.find env.locals name
    with Not_found -> 
    try 
      Hashtbl.find env.params name
    with Not_found -> 
    try
      Hashtbl.find env.globals name
    with Not_found -> failwith ("variable " ^ name ^ " : does not exist")

  let set_var env name value =
    try 
      let _ = Hashtbl.find env.locals name in
      Hashtbl.replace env.locals name value
    with Not_found -> 
    try 
      let _ = Hashtbl.find env.params name in
      Hashtbl.replace env.params name value
    with Not_found -> 
    try
      let _ = Hashtbl.find env.globals name in
      Hashtbl.replace env.globals name value
    with Not_found -> failwith ("variable " ^ name ^ " : does not exist")

  let get_fun env name =
    try
      Hashtbl.find env.fonctions name
    with Not_found -> failwith ("fonction " ^ name ^ " : does not exist")
end

exception EBadTyping

exception EReturn of value

let rec int_of_expr env e =
  match eval_expr env e with
  | Int(n) -> n
  | _ -> raise EBadTyping

and bool_of_expr env e =
  match eval_expr env e with
  | Bool(b) -> b
  | _ -> raise EBadTyping

and eval_unary env op e =
  match op with 
  | Minus -> let v = int_of_expr env e in Int(-v)
  | Not -> let v = bool_of_expr env e in Bool(not v)

and eval_binary_op env op e1 e2 =
  match op with
  | Add -> 
    begin
      let v1 = int_of_expr env e1 in
      let v2 = int_of_expr env e2 in
      Int(v1 + v2)
    end
  | Sub ->
    begin
      let v1 = int_of_expr env e1 in
      let v2 = int_of_expr env e2 in
      Int(v1 - v2)
    end
  | Mul ->
    begin
      let v1 = int_of_expr env e1 in
      let v2 = int_of_expr env e2 in
      Int(v1 * v2)
    end
  | Div ->
    begin
      let v1 = int_of_expr env e1 in
      let v2 = int_of_expr env e2 in
      Int(v1 / v2)
    end
  | Mod ->
    begin
      let v1 = int_of_expr env e1 in
      let v2 = int_of_expr env e2 in
      Int(v1 mod v2)
    end
  | Lt ->
    begin
      let v1 = int_of_expr env e1 in
      let v2 = int_of_expr env e2 in
      Bool(v1 < v2)
    end
  | And ->
    if bool_of_expr env e1
    then Bool(bool_of_expr env e2)
    else Bool(false)
  | Or ->
    if bool_of_expr env e1
    then Bool(true)
    else Bool(bool_of_expr env e2)
  | Eq ->
    begin
      match eval_expr env e1, eval_expr env e2 with
      | Int(n1), Int(n2) -> Bool(n1 = n2)
      | Bool(b1), Bool(b2) -> Bool(b1 = b2)
      | _ -> raise EBadTyping
    end

and eval_expr env e =
  match e with
  | Cst(n) -> Int(n)
  | True -> Bool(true)
  | False -> Bool(false)
  | Unary(op, e) -> eval_unary env op e
  | Binary(op, e1, e2) -> eval_binary_op env op e1 e2
  | Get(n) -> Env.get_var env n
  | Call(n, p) -> 
    begin
      let f = Env.get_fun env n in
      let params = Hashtbl.create (List.length p) in
      List.iter2 
        (Hashtbl.add params)
        (fst (List.split f.params))
        (List.map (eval_expr env) p)
      ;
      let locals = Hashtbl.create (List.length f.locals) in
      List.iter
        (fun (n, _) -> Hashtbl.add locals n Undefined)
        f.locals
      ;
      let (env' : Env.t) = {
        fonctions = env.fonctions;
        globals = env.globals;
        locals = locals;
        params = params;
      } in
      try
        let _ = eval_seq env' f.code in
        Undefined
      with EReturn(v) -> v
    end

and eval_instr env i =
  match i with
  | Putchar(e) ->
    begin
      let n = int_of_expr env e in
      (* print_int n; *)
      print_char (Char.chr (n));
      env
    end
  | Set(n, e) ->
    begin
      Env.set_var env n (eval_expr env e);
      env
    end
  | If(e, s1, s2) ->
    if bool_of_expr env e
    then eval_seq env s1
    else eval_seq env s2
  | While(e, s) ->
    if bool_of_expr env e
    then let env' = eval_seq env s in eval_instr env' i
    else env    
  | Return(e) -> raise (EReturn(eval_expr env e))
  | Expr(e) ->
    begin
      let _ = eval_expr env e in
      env
    end

and eval_seq env s =
  List.fold_left eval_instr env s

and interpret prog =
  let (env : Env.t) = {
    fonctions = 
      Hashtbl.of_seq 
        (List.to_seq (List.map (fun f -> (f.name, f)) prog.functions));
    globals = 
      Hashtbl.of_seq 
        (List.to_seq (List.map (fun (n, _) -> (n, Undefined)) prog.globals));
    locals = Hashtbl.create 0;
    params = Hashtbl.create 0;
  } in
  eval_expr env (Call("main", []))
