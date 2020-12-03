{
    open Imp_parser

    let key_words = Hashtbl.create 20

    let _ = Hashtbl.add key_words "if" IF
}

let ident = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*
let integer = ('-' | '+')? ['1'-'9'] ['0'-'9']*

rule scan = parse
    | _ as c
    {
        Printf.printf "%c" c
    }

    | eof
    {
        ()
    }