open Flowlog;;

type token =
  | EOF
  | CLAUSE_NAME of (string)
  | LPAREN
  | RPAREN
  | COLON_HYPHEN
  | SEMICOLON
  | COMMA
  | CONSTANT of (string)
  | VARIABLE of (string)
  | NOT
  | EQUALS
  | BOOLEAN of (bool)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Flowlog.relation list
