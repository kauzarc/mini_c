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
      let start_include = "#include \"" ^ Sys.argv.(1) ^ "\"\n" in
      let buffer = preproc (Buffer.create 1000) (Lexing.from_string start_include) in
      let lexbuf = Lexing.from_string (Buffer.contents buffer) in
      let prog = prog scan lexbuf in
      printf "program: \n%s\n" (string_of_prog prog);
      if well_typed prog
      then 
        begin
          printf "no type error\n";
          printf "evaluation:\n";
          let _ = interpret prog in
          printf "success\n"
        end
      else failwith "type error"
    end

let _ = main ()