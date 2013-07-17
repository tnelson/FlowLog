open Flowlog_Types.Syntax;;
open Type_Helpers.Parsing;;
type token =
  | EOF
  | IMPORT
  | SEMICOLON
  | NAME of (string)
  | DOUBLEQUOTE
  | PERIOD
  | BLACKBOX
  | AMPERSAND
  | NUMBER of (string)
  | DOTTED_IP of (string)
  | MODULE
  | COLON
  | TYPE
  | EQUALS
  | LCURLY
  | RCURLY
  | COMMA
  | LPAREN
  | RPAREN
  | COLON_HYPHEN
  | NOT
  | BOOLEAN of (bool)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> program
