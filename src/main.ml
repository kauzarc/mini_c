let print_imp_token () = 
  let file_chanel_in = open_in "example.imp" in
  let lexbuf = Lexing.from_channel file_chanel_in in
  let token_map = Hashtbl.create 20 in
  let _ = 
    Hashtbl.add token_map Imp_parser.PAR_O "PAR_O";
    Hashtbl.add token_map Imp_parser.PAR_F "PAR_F";
    Hashtbl.add token_map Imp_parser.BR_O "BR_O";
    Hashtbl.add token_map Imp_parser.BR_F "BR_F";
    Hashtbl.add token_map Imp_parser.PLUS "PLUS";
    Hashtbl.add token_map Imp_parser.FOIS "FOIS";
    Hashtbl.add token_map Imp_parser.LT "LT";
    Hashtbl.add token_map Imp_parser.EQUAL "EQUAL";
    Hashtbl.add token_map Imp_parser.PUTCHAR "PUTCHAR";
    Hashtbl.add token_map Imp_parser.IF "IF";
    Hashtbl.add token_map Imp_parser.ELSE "ELSE";
    Hashtbl.add token_map Imp_parser.WHILE "WHILE";
    Hashtbl.add token_map Imp_parser.RETURN "RETURN";
    Hashtbl.add token_map Imp_parser.VOID "VOID";
    Hashtbl.add token_map Imp_parser.INT "INT";
    Hashtbl.add token_map Imp_parser.BOOL "BOOL";
    Hashtbl.add token_map Imp_parser.COMMA "COMMA";
    Hashtbl.add token_map Imp_parser.SEMI "SEMI"
  in
  let rec printer () =
    match Imp_lexer.scan lexbuf with
    | Imp_parser.CONST(n) -> Printf.printf "CONST %d\n" n; printer ()
    | Imp_parser.ID(s) -> Printf.printf "ID %s\n" s; printer ()
    | Imp_parser.EOF -> Printf.printf "EOF\n"
    | token -> Printf.printf "%s\n" (Hashtbl.find token_map token); printer ()
  in
  printer ()

let print_imp_abr prog =
  let rec print_prog (p : Imp.prog) =
    Printf.printf "----------GLOBALS:----------\n";
    List.iter print_var p.globals;
    Printf.printf "\n----------FONCTION:----------\n";
    List.iter print_fn p.functions

  and print_var (n, t) = 
    Printf.printf "var %s : %s\n" n (typ_to_string t)

  and print_fn (fn : Imp.fun_def) =
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
    | Imp.Putchar(e) -> Printf.printf "Putchar(%s);\n" (expr_to_string e)
    | Imp.Set(s, e) -> Printf.printf "Set(%s, %s);\n" s (expr_to_string e)
    | Imp.If(e, s1, s2) ->
      Printf.printf "If(%s,\n[" (expr_to_string e);
      List.iter print_instr s1;
      Printf.printf "], \n[";
      List.iter print_instr s2;
      Printf.printf "])\n"
    | Imp.While(e, s) ->
      Printf.printf "While(%s,\n[" (expr_to_string e);
      List.iter print_instr s;
      Printf.printf "])\n";
    | Imp.Return(e) -> Printf.printf "Return(%s)\n" (expr_to_string e) 
    | Imp.Expr(e) -> Printf.printf "Expr(%s)\n" (expr_to_string e) 

  and expr_to_string expr =
    match expr with
    | Imp.Cst(n) -> "Cst(" ^ (string_of_int n) ^ ")"
    | Imp.Add(e1, e2) -> "Add(" ^ (expr_to_string e1) ^ ", " ^ (expr_to_string e2) ^ ")"
    | Imp.Mul(e1, e2) -> "Mul(" ^ (expr_to_string e1) ^ ", " ^ (expr_to_string e2) ^ ")"
    | Imp.Lt(e1, e2) -> "Lt(" ^ (expr_to_string e1) ^ ", " ^ (expr_to_string e2) ^ ")"
    | Imp.Get(s) -> "Get(" ^ s ^ ")"
    | Imp.Call(s, l) -> "Call(" ^ s ^ ", [" ^ (List.fold_left (fun acc e -> acc ^ (expr_to_string e) ^ ", ") "" l) ^ "])"

  and typ_to_string t =
    match t with
    | Imp.Void -> "void"
    | Imp.Int -> "int"
    | Imp.Bool -> "bool"
  in
  print_prog prog

let _ =
  let file_chanel_in = open_in "example.imp" in
  let lexbuf = Lexing.from_channel file_chanel_in in
  let prog = Imp_parser.prog Imp_lexer.scan lexbuf in
  print_imp_abr prog;
  if Imp.well_typed_prog prog
  then Printf.printf "well typed !\n"
  else Printf.printf "not well typed !\n"