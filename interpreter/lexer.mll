{
open Parser;;       (* The type token is defined in parser.mli *)
}
let name_char = ['a'-'z''A'-'Z''_''/''+''-']
rule token = parse
    [' ' '\t'] { token lexbuf }     (* skip blanks *)
  | eof { EOF }
  | ('+'['a'-'z'] | '-'['a'-'z'] | ['a'-'z'])(name_char*) as name { CLAUSE_NAME(name) }
  | ')' { RPAREN }
  | ":-" { COLON_HYPHEN }
  | ';' { SEMICOLON }
  | ',' { COMMA }
  | ['0'-'9']+ | '0''x'(['0'-'9']+) as constant { CONSTANT(constant) }
  | ['A'-'Z'](name_char*) as variable { VARIABLE(variable) }
  | "not" { NOT }
  | '=' { EQUALS }
  | "true" | "false" as boolean { BOOLEAN(boolean = "true") }