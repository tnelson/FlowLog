{
open Parser;;       (* The type token is defined in parser.mli *)
}
rule token = parse
    [' ' '\t' '\n' '\r'] { token lexbuf }
  | eof { EOF }
  | "import" { IMPORT }
  | '.' { PERIOD }
  | "blackbox" { BLACKBOX }
  | '@' { AMPERSAND }
  | "module" { MODULE }
  | ":-" { COLON_HYPHEN }
  | ':' { COLON }
  | "type" { TYPE }
  | '=' { EQUALS }
  | '{' { LCURLY }
  | '}' { RCURLY }
  | ',' { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "not" { NOT }
  | "true" | "false" as boolean { BOOLEAN(boolean = "true") }
  | ['0'-'9']+ | '0''x'(['0'-'9']+) as number { NUMBER(number) }
  | ['a'-'z''A'-'Z''_''+''-''0'-'9']+ as name { NAME(String.lowercase name) }