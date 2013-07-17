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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Flowlog_Types.Syntax;;
  open Type_Helpers.Parsing;;

  let parse_debug = false;;
# 33 "../parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* IMPORT *);
  258 (* SEMICOLON *);
  260 (* DOUBLEQUOTE *);
  261 (* PERIOD *);
  262 (* BLACKBOX *);
  263 (* AMPERSAND *);
  266 (* MODULE *);
  267 (* COLON *);
  268 (* TYPE *);
  269 (* EQUALS *);
  270 (* LCURLY *);
  271 (* RCURLY *);
  272 (* COMMA *);
  273 (* LPAREN *);
  274 (* RPAREN *);
  275 (* COLON_HYPHEN *);
  276 (* NOT *);
    0|]

let yytransl_block = [|
  259 (* NAME *);
  264 (* NUMBER *);
  265 (* DOTTED_IP *);
  277 (* BOOLEAN *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\003\000\003\000\003\000\
\003\000\004\000\005\000\005\000\006\000\007\000\008\000\008\000\
\009\000\009\000\009\000\009\000\010\000\010\000\011\000\012\000\
\012\000\013\000\013\000\014\000\014\000\014\000\014\000\014\000\
\014\000\015\000\015\000\015\000\015\000\016\000\016\000\000\000"

let yylen = "\002\000\
\004\000\001\000\001\000\002\000\002\000\001\000\001\000\002\000\
\002\000\003\000\006\000\003\000\003\000\007\000\001\000\003\000\
\007\000\007\000\006\000\009\000\001\000\003\000\003\000\001\000\
\003\000\001\000\002\000\003\000\004\000\003\000\006\000\005\000\
\001\000\001\000\001\000\003\000\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\040\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\005\000\010\000\012\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\000\000\000\000\001\000\008\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\011\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\023\000\000\000\016\000\
\000\000\000\000\035\000\000\000\033\000\000\000\000\000\026\000\
\000\000\000\000\000\000\022\000\000\000\000\000\000\000\000\000\
\000\000\027\000\019\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\030\000\000\000\000\000\036\000\025\000\
\028\000\018\000\017\000\000\000\014\000\000\000\000\000\000\000\
\029\000\000\000\032\000\000\000\037\000\039\000\020\000\031\000"

let yydgoto = "\002\000\
\005\000\006\000\021\000\007\000\008\000\012\000\022\000\034\000\
\023\000\035\000\036\000\054\000\055\000\056\000\057\000\078\000"

let yysindex = "\011\000\
\042\255\000\000\014\255\020\255\000\000\034\255\042\255\042\255\
\038\255\044\255\046\255\030\255\000\000\000\000\000\000\000\000\
\052\255\051\255\047\255\060\255\055\000\030\255\030\255\057\255\
\000\000\010\255\053\255\000\000\000\000\000\000\065\255\036\255\
\049\255\054\255\055\255\058\255\056\255\000\000\066\255\068\255\
\001\255\059\255\061\255\072\255\068\255\000\000\063\255\000\000\
\015\255\073\255\000\000\003\255\000\000\075\255\067\255\000\000\
\069\255\001\255\001\255\000\000\070\255\071\255\078\255\011\255\
\080\255\000\000\000\000\001\255\033\255\083\255\085\255\074\255\
\087\255\077\255\086\255\000\000\076\255\079\255\000\000\000\000\
\000\000\000\000\000\000\001\255\000\000\027\255\092\255\033\255\
\000\000\088\255\000\000\081\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\090\255\091\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\096\000\098\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\084\255\
\000\000\000\000\000\000\089\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\035\255\000\000\
\093\255\000\000\000\000\000\000\000\000\000\000\101\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\053\255\000\255\000\000\094\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\050\000\037\000\000\000\000\000\000\000\000\000\250\255\
\000\000\000\000\060\000\198\255\000\000\053\000\195\255\224\255"

let yytablesize = 112
let yytable = "\070\000\
\071\000\034\000\077\000\049\000\050\000\049\000\050\000\081\000\
\051\000\080\000\051\000\001\000\032\000\075\000\050\000\034\000\
\009\000\034\000\051\000\063\000\052\000\053\000\010\000\053\000\
\077\000\090\000\077\000\033\000\076\000\075\000\050\000\064\000\
\019\000\048\000\051\000\075\000\050\000\060\000\062\000\015\000\
\051\000\020\000\003\000\011\000\091\000\016\000\039\000\004\000\
\018\000\015\000\017\000\040\000\015\000\092\000\028\000\094\000\
\013\000\014\000\029\000\030\000\024\000\025\000\027\000\026\000\
\031\000\037\000\038\000\041\000\046\000\045\000\047\000\042\000\
\043\000\044\000\032\000\065\000\067\000\058\000\040\000\059\000\
\074\000\069\000\068\000\079\000\082\000\073\000\083\000\072\000\
\085\000\095\000\087\000\088\000\084\000\086\000\093\000\006\000\
\089\000\007\000\096\000\002\000\003\000\015\000\024\000\061\000\
\066\000\034\000\021\000\000\000\000\000\000\000\000\000\038\000"

let yycheck = "\058\000\
\059\000\002\001\064\000\003\001\004\001\003\001\004\001\069\000\
\008\001\068\000\008\001\001\000\003\001\003\001\004\001\016\001\
\003\001\018\001\008\001\005\001\020\001\021\001\003\001\021\001\
\086\000\084\000\088\000\018\001\018\001\003\001\004\001\017\001\
\003\001\040\000\008\001\003\001\004\001\044\000\045\000\002\001\
\008\001\012\001\001\001\010\001\018\001\002\001\011\001\006\001\
\003\001\015\001\007\001\016\001\018\001\086\000\000\000\088\000\
\007\000\008\000\022\000\023\000\009\001\011\001\003\001\017\001\
\008\001\013\001\002\001\019\001\003\001\014\001\003\001\018\001\
\018\001\016\001\003\001\003\001\002\001\019\001\016\001\019\001\
\003\001\013\001\016\001\004\001\002\001\015\001\002\001\018\001\
\002\001\002\001\005\001\016\001\019\001\017\001\003\001\000\000\
\018\001\000\000\018\001\010\001\010\001\018\001\002\001\044\000\
\052\000\013\001\018\001\255\255\255\255\255\255\255\255\018\001"

let yynames_const = "\
  EOF\000\
  IMPORT\000\
  SEMICOLON\000\
  DOUBLEQUOTE\000\
  PERIOD\000\
  BLACKBOX\000\
  AMPERSAND\000\
  MODULE\000\
  COLON\000\
  TYPE\000\
  EQUALS\000\
  LCURLY\000\
  RCURLY\000\
  COMMA\000\
  LPAREN\000\
  RPAREN\000\
  COLON_HYPHEN\000\
  NOT\000\
  "

let yynames_block = "\
  NAME\000\
  NUMBER\000\
  DOTTED_IP\000\
  BOOLEAN\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string list * blackbox list) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : notif_type list * clause list) in
    Obj.repr(
# 49 "parser.mly"
                                 ( match _1 with (imports, blackboxes) ->
        match _3 with (types, clauses) ->
        make_Program _2 imports blackboxes types clauses )
# 198 "../parser.ml"
               : program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "parser.mly"
             ( ([_1], []) )
# 205 "../parser.ml"
               : string list * blackbox list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : blackbox) in
    Obj.repr(
# 55 "parser.mly"
               ( ([], [_1]) )
# 212 "../parser.ml"
               : string list * blackbox list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string list * blackbox list) in
    Obj.repr(
# 56 "parser.mly"
                 ( match _2 with (imports, blackboxes) -> (_1 :: imports, blackboxes) )
# 220 "../parser.ml"
               : string list * blackbox list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : blackbox) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string list * blackbox list) in
    Obj.repr(
# 57 "parser.mly"
                   ( match _2 with (imports, blackboxes) -> (imports, _1 :: blackboxes) )
# 228 "../parser.ml"
               : string list * blackbox list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : notif_type) in
    Obj.repr(
# 60 "parser.mly"
                ( ([_1], []) )
# 235 "../parser.ml"
               : notif_type list * clause list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : clause) in
    Obj.repr(
# 61 "parser.mly"
             ( ([], [_1]) )
# 242 "../parser.ml"
               : notif_type list * clause list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : notif_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : notif_type list * clause list) in
    Obj.repr(
# 62 "parser.mly"
                       ( match _2 with (types, clauses) -> (_1 :: types, clauses) )
# 250 "../parser.ml"
               : notif_type list * clause list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : clause) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : notif_type list * clause list) in
    Obj.repr(
# 63 "parser.mly"
                    ( match _2 with (types, clauses) -> (types, _1 :: clauses) )
# 258 "../parser.ml"
               : notif_type list * clause list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 66 "parser.mly"
                            ( make_import _2 )
# 265 "../parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 69 "parser.mly"
                                                         ( make_External_BB (String.lowercase _2) _4 (int_of_string _5) )
# 274 "../parser.ml"
               : blackbox))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 70 "parser.mly"
                              ( make_Internal_BB (String.lowercase _2) )
# 281 "../parser.ml"
               : blackbox))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 73 "parser.mly"
                        ( String.lowercase _2 )
# 288 "../parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string list) in
    Obj.repr(
# 76 "parser.mly"
                                                       ( make_Type (String.lowercase _2) _5 )
# 296 "../parser.ml"
               : notif_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "parser.mly"
           ( [String.uppercase _1] )
# 303 "../parser.ml"
               : string list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string list) in
    Obj.repr(
# 80 "parser.mly"
                           ( (String.uppercase _1) :: _3 )
# 311 "../parser.ml"
               : string list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : argument list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : literal list) in
    Obj.repr(
# 83 "parser.mly"
                                                                                 ( make_Plus_Minus_Clause (String.lowercase _1) _3 _6 )
# 320 "../parser.ml"
               : clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : literal list) in
    Obj.repr(
# 84 "parser.mly"
                                                                       ( make_HelperClause (String.lowercase _1) (List.map (fun str -> make_Arg_term(make_Variable(str))) _3) _6 )
# 329 "../parser.ml"
               : clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : literal list) in
    Obj.repr(
# 85 "parser.mly"
                                                             ( make_HelperClause (String.lowercase _1) [] _5 )
# 337 "../parser.ml"
               : clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : argument) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : argument) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : literal list) in
    Obj.repr(
# 86 "parser.mly"
                                                                                       ( make_NotifClause (String.lowercase _1) [_3; _5] _8 )
# 347 "../parser.ml"
               : clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : argument) in
    Obj.repr(
# 89 "parser.mly"
                ( [_1] )
# 354 "../parser.ml"
               : argument list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : argument) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string list) in
    Obj.repr(
# 90 "parser.mly"
                                ( _1 :: List.map (fun str -> make_Arg_term (make_Variable str)) _3 )
# 362 "../parser.ml"
               : argument list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 93 "parser.mly"
                      ( make_Arg_notif (make_Notif_var (String.lowercase _3) (String.uppercase _1)) )
# 370 "../parser.ml"
               : argument))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : literal) in
    Obj.repr(
# 96 "parser.mly"
              ( [_1] )
# 377 "../parser.ml"
               : literal list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : literal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : literal list) in
    Obj.repr(
# 97 "parser.mly"
                                 ( _1 :: _3 )
# 385 "../parser.ml"
               : literal list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : atom) in
    Obj.repr(
# 100 "parser.mly"
           ( Pos(_1) )
# 392 "../parser.ml"
               : literal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : atom) in
    Obj.repr(
# 101 "parser.mly"
               ( Neg(_2) )
# 399 "../parser.ml"
               : literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : term) in
    Obj.repr(
# 104 "parser.mly"
                       ( Equals(_1, _3) )
# 407 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : term list) in
    Obj.repr(
# 105 "parser.mly"
                                   ( make_Apply (String.lowercase _1) _3 )
# 415 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 106 "parser.mly"
                         ( make_Apply (String.lowercase _1) [] )
# 422 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : term list) in
    Obj.repr(
# 107 "parser.mly"
                                               ( make_Apply_Query (String.lowercase _1) _3 _5 )
# 431 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 108 "parser.mly"
                                     ( make_Apply_Query (String.lowercase _1) _3 [] )
# 439 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 109 "parser.mly"
              ( Bool(_1) )
# 446 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 112 "parser.mly"
           ( (if parse_debug then Printf.printf "NAME: %s\n%!" _1; make_Constant_or_Variable (String.uppercase _1)) )
# 453 "../parser.ml"
               : term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 113 "parser.mly"
             ( (if parse_debug then Printf.printf "NUMBER: %s\n%!" _1; Constant(_1)) )
# 460 "../parser.ml"
               : term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 114 "parser.mly"
                                   ( (if parse_debug then Printf.printf "DQ NAME DQ: %s\n%!" _2; Constant(_2)) )
# 467 "../parser.ml"
               : term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 115 "parser.mly"
                       ( make_Field_ref (String.uppercase _1) (String.uppercase _3) )
# 475 "../parser.ml"
               : term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : term) in
    Obj.repr(
# 118 "parser.mly"
           ( [_1] )
# 482 "../parser.ml"
               : term list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : term list) in
    Obj.repr(
# 119 "parser.mly"
                           ( _1 :: _3 )
# 490 "../parser.ml"
               : term list))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : program)
