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

let _ =
  print_imp_token ()