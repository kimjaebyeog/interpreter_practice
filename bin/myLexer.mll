{
  open MyParser
}

let digit = ['0'-'9']
let space = ' ' | '\t' | '\r' | '\n'
let alpha = ['a'-'z' 'A'-'Z' '_' ] 
let ident = alpha (alpha | digit)* 

rule token = parse
| space+      { token lexbuf }
| "let"       { LET }
| "rec"       { REC }
| "and"       { AND }
| "in"        { IN  }
| "fun"       { FUN  }
| "->"        { RARROW }
| "if"        { IF }
| "then"      { THEN }
| "else"      { ELSE }
| "match"     { MATCH }
| "with"      { WITH }
| "end"       { END }
| "_"         { UNDERSCORE }
| '|'         { BAR }
| '='         { EQ }
| '<'         { LT }
| '+'         { PLUS }
| '-'         { MINUS }
| '*'         { MUL }
| '/'         { DIV }
| ','         { COMMA }
| '('         { LPAR }
| ')'         { RPAR }
| '['         { LBRK }
| ']'         { RBRK }
| "::"        { DCOLON }
| ';'         { SEMI }
| ";;"        { DSEMI }
| "true"      { TRUE }
| "false"     { FALSE }
| digit+ as n { INT (int_of_string n) }
| ident  as n { ID n }
| eof         { EOF  }
| _           { failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf)}
