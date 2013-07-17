{
open Parser;;       (* The type token is defined in parser.mli *)
}
rule token = parse
    [' ' '\t' '\n' '\r'] { token lexbuf }    
  | "//" [^ '\n']* { token lexbuf }
  | eof { EOF }
  | "import" { IMPORT }
  | '.' { PERIOD }
  | "blackbox" { BLACKBOX }
  | '@' { AMPERSAND }
  | "module" { MODULE }
  | ":-" { COLON_HYPHEN }
  | ':' { COLON }
  | ';' { SEMICOLON }
  | "type" { TYPE }
  | '=' { EQUALS }
  | '{' { LCURLY }
  | '}' { RCURLY }
  | ',' { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '"' { DOUBLEQUOTE }
  | "not" { NOT }
  | "true" | "false" as boolean { BOOLEAN(boolean = "true") }
  | ['0'-'9']?['0'-'9']?['0'-'9']"."['0'-'9']?['0'-'9']?['0'-'9']"."['0'-'9']?['0'-'9']?['0'-'9']"."['0'-'9']?['0'-'9']?['0'-'9'] as dotted_ip { DOTTED_IP(dotted_ip)}
  | ['0'-'9']+ | '0''x'(['0'-'9']+) as number { NUMBER(number) }
  | ['a'-'z''A'-'Z''_''+''-''0'-'9']+ as name { NAME(name) }
