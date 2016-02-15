rule token = parse
[' ' '\t']     { token lexbuf }     (* skip blanks *)
  | ['\n' ]        { EOL }
  | ['0'-'9']'.'+ as lxm { INT(int_of_string lxm) }
  | ['R''B''N''K''Q'['a'-'h']?[1-8]?['a'-'h'][1-8] ->
  | eof            { raise Eof }
