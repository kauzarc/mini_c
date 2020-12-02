rule scan = parse
    | _ as c
    {
        Printf.printf "%c" c
    }

    | eof
    {
        ()
    }