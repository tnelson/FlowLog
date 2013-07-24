open Type_Helpers;;
open Types.Types;;
type token =
  | EOF
  | IMPORT
  | BLACKBOX
  | MODULE
  | TYPE
  | PLUS
  | MINUS
  | HELPER
  | ACTION
  | NOT
  | BOOLEAN of (bool)
  | PERIOD
  | AMPERSAND
  | COLON_HYPHEN
  | COLON
  | SEMICOLON
  | EQUALS
  | LCURLY
  | RCURLY
  | COMMA
  | LPAREN
  | RPAREN
  | DOUBLEQUOTE
  | DOTTED_IP of (string)
  | NUMBER of (string)
  | NAME of (string)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> program
