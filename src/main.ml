open Lexer
open Parser
open Type
open Mc
open Printf
open Interpreter

let main () =
  if Array.length Sys.argv <> 2
  then printf "ERROR:\nusage: %s file.mc\n" Sys.argv.(0)
  else 
    begin
      let file_chanel_in = open_in Sys.argv.(1) in
      let lexbuf = Lexing.from_channel file_chanel_in in
      let prog = prog scan lexbuf in
      printf "program: \n%s\n\n" (string_of_prog prog);
      if well_typed prog
      then 
        begin
          printf "well typed program !\n";
          printf "evaluation:\n";
          let _ = interpret prog in
          printf "\nsuccess\n"
        end
      else failwith "type error"
    end

let _ = main ()