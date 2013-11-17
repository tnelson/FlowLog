{
open Surface_Parser;;       (* The tokens are defined in this mli *)

(* NOTE: To get case-insensitivity, make sure to lowercase the stream before passing in. *)

(* TODO: We've likely hit the point where the table is blowing up because we have a lot of keywords. 
         Could be faster to use the hash-table lookup method for keywords? *)

(* COMMENTS --- the pattern below for multiline /* */ assumes no strings containing
  /* etc. Standard "dumb" comment assumptions due to limited language syntax. *)

let any_counter = ref 0;;

}
rule token = parse
    [' ' '\t' '\r'] { token lexbuf }    
  | "\n" { Lexing.new_line lexbuf; token lexbuf }
  | "//" [^ '\n']* { token lexbuf }
  | "/*" {commenting lexbuf}
  | eof { EOF }

  | "include" { INCLUDE }
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
  | "then" { THEN }
  | "on" { ON }

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
  | "!=" { NOTEQUALS } 
  | '{' { LCURLY }
  | '}' { RCURLY }
  | ',' { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }  

  | "any" { any_counter := !any_counter+1; NAME("any"^(string_of_int !any_counter)) }

  | ['0'-'9']?['0'-'9']?['0'-'9']"."['0'-'9']?['0'-'9']?['0'-'9']"."['0'-'9']?['0'-'9']?['0'-'9']"."['0'-'9']?['0'-'9']?['0'-'9'] as dotted_ip { DOTTED_IP(dotted_ip)}
  | ['0'-'9']+ | '0''x'(['0'-'9''a'-'f']+) as number { NUMBER(number) }
  | ['a'-'z''A'-'Z''_''0'-'9']+ as name { NAME(name) }
  | ['a'-'z''A'-'Z''_''0'-'9''-']+ as name 
    { Printf.printf "Bad identifier: %s. Cannot use dashes. Use underscore instead.\n%!" name; 
      token lexbuf; }    
  | '"'['.''/''\\''a'-'z''A'-'Z''_''0'-'9''-']+'"' as id { QUOTED_IDENTIFIER(id) }
  | _ as c { Printf.printf "Unknown character: %c\n" c; token lexbuf;}
and commenting = parse 
  | "*/" { token lexbuf }
  | "\n" { Lexing.new_line lexbuf; commenting lexbuf }
  | eof { EOF }
  | _   { commenting lexbuf }
