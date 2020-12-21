{
    open Parser

    let key_words = 
        Hashtbl.of_seq (List.to_seq 
        [
            ("putchar", PUTCHAR);
            ("if", IF);
            ("else", ELSE);
            ("while", WHILE);
            ("return", RETURN);
            ("void", VOID);
            ("int", INT);
            ("bool", BOOL);
            ("for", FOR);
        ])
    
    let key_caracters =
        Hashtbl.of_seq (List.to_seq 
        [
            ('(', PAR_O);
            (')', PAR_F);
            ('{', BR_O);
            ('}', BR_F);
            ('+', PLUS);
            ('-', MINUS);
            ('*', FOIS);
            ('<', LT);
            ('=', EQUAL);
            (',', COMMA);
            (';', SEMI);
        ])

}

let ident = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*
let integer = (['1'-'9'] ['0'-'9']*) | '0'
let sep = [' ' '\n' '\t']

rule scan = parse
    | eof { EOF }
    | integer as i { CONST(int_of_string i) }
    | ident as id {
        try
            Hashtbl.find key_words id 
        with Not_found -> ID(id) 
    }
    | sep { scan lexbuf }
    | _ as c {
        try
            Hashtbl.find key_caracters c
        with Not_found -> failwith "bad caracter"
    }

(* let path = (ident '/')* ident ".mc"

rule include buffer = parse
    | "#include \"" path "\"\n" *)