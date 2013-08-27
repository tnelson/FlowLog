open Flowlog_Types;;
type token =
  | EOF
  | IMPORT
  | TABLE
  | REMOTE
  | OUTGOING
  | THEN
  | INCOMING
  | DO
  | AT
  | TIMEOUT
  | PURE
  | ON
  | SEND
  | TO
  | COLON_EQUALS
  | DELETE
  | INSERT
  | WHERE
  | EVENT
  | INTO
  | FROM
  | NOT
  | IMPLIES
  | TRUE
  | FALSE
  | IFF
  | OR
  | AND
  | PERIOD
  | COLON
  | SEMICOLON
  | EQUALS
  | NOTEQUALS
  | LCURLY
  | RCURLY
  | COMMA
  | LPAREN
  | RPAREN
  | DOUBLEQUOTE
  | BOOLEAN of (bool)
  | DOTTED_IP of (string)
  | NUMBER of (string)
  | NAME of (string)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> flowlog_ast
