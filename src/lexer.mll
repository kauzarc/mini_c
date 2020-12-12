{
    open Parser

    let key_words = Hashtbl.create 10
    let _ = 
        Hashtbl.add key_words "putchar" PUTCHAR;
        Hashtbl.add key_words "if" IF;
        Hashtbl.add key_words "else" ELSE;
        Hashtbl.add key_words "while" WHILE;
        Hashtbl.add key_words "return" RETURN;
        Hashtbl.add key_words "void" VOID;
        Hashtbl.add key_words "int" INT;
        Hashtbl.add key_words "bool" BOOL
    
    let key_caracters = Hashtbl.create 10
    let _ =
        Hashtbl.add key_caracters '(' PAR_O;
        Hashtbl.add key_caracters ')' PAR_F;
        Hashtbl.add key_caracters '{' BR_O;
        Hashtbl.add key_caracters '}' BR_F;
        Hashtbl.add key_caracters '+' PLUS;
        Hashtbl.add key_caracters '*' FOIS;
        Hashtbl.add key_caracters '<' LT;
        Hashtbl.add key_caracters '=' EQUAL;
        Hashtbl.add key_caracters ',' COMMA;
        Hashtbl.add key_caracters ';' SEMI

}

let ident = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*
let integer = (('-')? ['1'-'9'] ['0'-'9']*) | '0'
let sep = [' ' '\n' '\t']

rule scan = parse
    | eof
    {
        EOF
    }

    | integer as i
    {
        CONST(int_of_string i)
    }

    | ident as id
    {
        try
            Hashtbl.find key_words id
        with Not_found -> ID(id)
    }

    |sep
    {
        scan lexbuf
    }

    | _ as c
    {
        try
            Hashtbl.find key_caracters c
        with Not_found -> failwith "bad caracter"
    }