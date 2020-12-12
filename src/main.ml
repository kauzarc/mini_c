open Lexer
open Parser
open Type
open Mc
open Printf

let _ =
  let file_chanel_in = open_in "example.mc" in
  let lexbuf = Lexing.from_channel file_chanel_in in
  let prog = prog scan lexbuf in

  printf "%s\n" (prog_to_string prog);

  if well_typed prog
  then Printf.printf "well typed !\n"
  else Printf.printf "not well typed !\n"