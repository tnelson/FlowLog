{
open Surface_Parser       (* The tokens are defined in this mli *)
open Flowlog_Helpers
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

  | "in" { IN }
  | "not" { NOT }
  | "or" { OR }
  | "xor" { XOR }
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

  | ['0'-'9']?['0'-'9']?['0'-'9']"."['0'-'9']?['0'-'9']?['0'-'9']"."['0'-'9']?['0'-'9']?['0'-'9']"."['0'-'9']?['0'-'9']?['0'-'9']
    "/"['0'-'9']+ as ip_with_mask
    { match Str.split (Str.regexp "/") ip_with_mask with
        | addr::mask::[] ->
          IPMASK(nwaddr_to_int_string (Packet.ip_of_string addr), mask)
        | _ -> Printf.printf "Bad IP mask.\n%!"; 
               token lexbuf;
    }

  | ['0'-'9']?['0'-'9']?['0'-'9']"."['0'-'9']?['0'-'9']?['0'-'9']"."['0'-'9']?['0'-'9']?['0'-'9']"."['0'-'9']?['0'-'9']?['0'-'9'] as dotted_ip 
    { NUMBER(nwaddr_to_int_string (Packet.ip_of_string dotted_ip))}
  | ['0'-'9''a'-'f']?['0'-'9''a'-'f']":"['0'-'9''a'-'f']?['0'-'9''a'-'f']":"['0'-'9''a'-'f']?['0'-'9''a'-'f']":"
    ['0'-'9''a'-'f']?['0'-'9''a'-'f']":"['0'-'9''a'-'f']?['0'-'9''a'-'f']":"['0'-'9''a'-'f']?['0'-'9''a'-'f'] as mac
    { NUMBER(macaddr_to_int_string (Packet.mac_of_string mac))}
  | ['0'-'9']+ | '0''x'(['0'-'9''a'-'f']+) as number 
    { NUMBER(number) }  

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
