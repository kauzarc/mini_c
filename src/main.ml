open Imp

let _ =
  let file_chanel_in = open_in "example.imp" in
  let lexbuf = Lexing.from_channel file_chanel_in in
  let prog = Imp_parser.prog Imp_lexer.scan lexbuf in

  print prog;

  if well_typed prog
  then Printf.printf "well typed !\n"
  else Printf.printf "not well typed !\n"