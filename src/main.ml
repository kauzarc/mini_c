open Lexer
open Parser
open Mc

let _ =
  let file_chanel_in = open_in "example.mc" in
  let lexbuf = Lexing.from_channel file_chanel_in in
  let prog = prog scan lexbuf in

  print prog;

  if well_typed prog
  then Printf.printf "well typed !\n"
  else Printf.printf "not well typed !\n"