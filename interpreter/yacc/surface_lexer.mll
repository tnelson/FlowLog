{
open Parser;;       (* The type token is defined in parser.mli *)

(* NOTE: To get case-insensitivity, make sure to lowercase the stream before passing in. *)

(* TODO: We've likely hit the point where the table is blowing up because we have a lot of keywords. 
         Could be faster to use the hash-table lookup method for keywords? *)

}
rule token = parse
    [' ' '\t' '\n' '\r'] { token lexbuf }    
  | "//" [^ '\n']* { token lexbuf }
  | eof { EOF }

  | "import" { IMPORT }  
  | "do" { DO }
  | "into" { INTO }
  | "from" { FROM }
  | "insert" { INSERT }
  | "delete" { DELETE }
  | "table" { TABLE }
  | "remote" { REMOTE }
  | "at" { AT }
  | "to" { TO }
  | "send" { SEND }
  | "event" { EVENT }
  | "incoming" { INCOMING }
  | "outgoing" { OUTGOING }
  | "where" { WHERE }
  | "timeout" { TIMEOUT }
  | "pure" { PURE }

  | "not" { NOT }
  | "or" { OR }
  | "and" { AND }
  | "implies" { IMPLIES }
  | "iff" { IFF }
  | "true" { TRUE }
  | "false" { FALSE }
  
  | '.' { PERIOD }
  | ":=" { COLON_EQUALS }
  | ':' { COLON }
  | ';' { SEMICOLON }
  | '=' { EQUALS }
  | "<>" { NOTEQUALS }
  | '{' { LCURLY }
  | '}' { RCURLY }
  | ',' { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '"' { DOUBLEQUOTE }
  | ['0'-'9']?['0'-'9']?['0'-'9']"."['0'-'9']?['0'-'9']?['0'-'9']"."['0'-'9']?['0'-'9']?['0'-'9']"."['0'-'9']?['0'-'9']?['0'-'9'] as dotted_ip { DOTTED_IP(dotted_ip)}
  | ['0'-'9']+ | '0''x'(['0'-'9']+) as number { NUMBER(number) }
  | ['a'-'z''A'-'Z''_''0'-'9']+ as name { NAME(name) }
  | _ as c { Printf.printf "Unknown character: %c\n" c; token lexbuf;}
