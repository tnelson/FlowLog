{
open Parser;;       (* The type token is defined in parser.mli *)
}
let name_char = ['a'-'z''A'-'Z''_''/''+''-''0'-'9']
rule token = parse
    [' ' '\t' '\n' '\r'] { token lexbuf }
  | eof { EOF }
  | "not" { NOT }
  | "true" | "false" as boolean { BOOLEAN(boolean = "true") }
  | ('+'['a'-'z'] | '-'['a'-'z'] | ['a'-'z'])(name_char*) as name { CLAUSE_NAME(name) }
  | '=' { EQUALS }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ":-" { COLON_HYPHEN }
  | ';' { SEMICOLON }
  | ',' { COMMA }
  | ['0'-'9']+ | '0''x'(['0'-'9']+) as constant { CONSTANT(constant) }
  | ['A'-'Z'](name_char*) as variable { VARIABLE(variable) }