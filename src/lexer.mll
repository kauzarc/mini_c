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
            ('/', SLASH);
            ('<', LT);
            ('>', MT);
            ('=', EQUAL);
            ('!', EXCM);
            ('&', AND);
            ('|', OR);
            (',', COMMA);
            (';', SEMI);
        ])

}

let ident = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*
let integer = (['1'-'9'] ['0'-'9']*) | '0'
let sep = [' ' '\n' '\t']

rule scan = parse
    | "==" { DBLEQUAL }
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

and preproc buffer = parse
    | "#include \"" (((ident | "." | "..") '/')* as dirpath) ((ident ".mc") as filename) "\"\n" 
        {
            let current = Sys.getcwd () in
            Sys.chdir dirpath;
            let file_chanel_in = open_in filename in
            let buffer = preproc buffer (Lexing.from_channel file_chanel_in) in
            Sys.chdir current;
            preproc buffer lexbuf
        }
    | _ as c { Buffer.add_char buffer c; preproc buffer lexbuf }
    | eof { buffer } 