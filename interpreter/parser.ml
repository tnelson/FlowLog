type token =
  | EOF
  | IMPORT
  | SEMICOLON
  | NAME of (string)
  | PERIOD
  | BLACKBOX
  | AMPERSAND
  | NUMBER of (string)
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
# 29 "../parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* IMPORT *);
  258 (* SEMICOLON *);
  260 (* PERIOD *);
  261 (* BLACKBOX *);
  262 (* AMPERSAND *);
  264 (* MODULE *);
  265 (* COLON *);
  266 (* TYPE *);
  267 (* EQUALS *);
  268 (* LCURLY *);
  269 (* RCURLY *);
  270 (* COMMA *);
  271 (* LPAREN *);
  272 (* RPAREN *);
  273 (* COLON_HYPHEN *);
  274 (* NOT *);
    0|]

let yytransl_block = [|
  259 (* NAME *);
  263 (* NUMBER *);
  275 (* BOOLEAN *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\003\000\003\000\003\000\
\003\000\004\000\005\000\005\000\006\000\007\000\008\000\008\000\
\009\000\009\000\009\000\009\000\010\000\010\000\011\000\012\000\
\012\000\013\000\013\000\014\000\014\000\014\000\014\000\014\000\
\014\000\015\000\015\000\016\000\016\000\000\000"

let yylen = "\002\000\
\004\000\001\000\001\000\002\000\002\000\001\000\001\000\002\000\
\002\000\003\000\006\000\003\000\003\000\007\000\001\000\003\000\
\007\000\007\000\006\000\008\000\001\000\003\000\003\000\001\000\
\003\000\001\000\002\000\003\000\004\000\003\000\006\000\005\000\
\001\000\001\000\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\038\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\005\000\010\000\012\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\000\000\000\000\001\000\008\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\011\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\000\000\016\000\000\000\000\000\033\000\000\000\000\000\026\000\
\000\000\000\000\000\000\022\000\000\000\000\000\000\000\000\000\
\027\000\019\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\030\000\000\000\000\000\025\000\028\000\018\000\
\017\000\000\000\014\000\000\000\000\000\000\000\029\000\020\000\
\032\000\000\000\035\000\037\000\031\000"

let yydgoto = "\002\000\
\005\000\006\000\021\000\007\000\008\000\012\000\022\000\034\000\
\023\000\035\000\036\000\054\000\055\000\056\000\057\000\077\000"

let yysindex = "\005\000\
\004\255\000\000\043\255\044\255\000\000\040\255\004\255\004\255\
\047\255\030\255\048\255\028\255\000\000\000\000\000\000\000\000\
\045\255\041\255\038\255\051\255\055\000\028\255\028\255\049\255\
\000\000\008\255\046\255\000\000\000\000\000\000\056\255\020\255\
\042\255\050\255\052\255\025\255\053\255\000\000\057\255\058\255\
\254\254\054\255\055\255\060\255\058\255\059\255\058\255\000\000\
\062\255\000\000\026\255\255\254\000\000\061\255\063\255\000\000\
\067\255\254\254\254\254\000\000\064\255\066\255\070\255\009\255\
\000\000\000\000\254\254\071\255\065\255\068\255\254\254\078\255\
\069\255\079\255\000\000\072\255\073\255\000\000\000\000\000\000\
\000\000\080\255\000\000\010\255\082\255\071\255\000\000\000\000\
\000\000\074\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\083\255\084\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\062\000\064\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\077\255\
\000\000\000\000\000\000\081\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\024\255\000\000\076\255\000\000\000\000\000\000\086\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\085\255\005\255\000\000\087\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\035\000\022\000\000\000\000\000\000\000\000\000\219\255\
\000\000\000\000\058\000\212\255\000\000\043\000\192\255\205\255"

let yytablesize = 103
let yytable = "\076\000\
\051\000\051\000\050\000\079\000\003\000\001\000\034\000\060\000\
\004\000\062\000\032\000\074\000\074\000\069\000\070\000\052\000\
\053\000\053\000\034\000\076\000\034\000\076\000\078\000\033\000\
\075\000\089\000\082\000\044\000\039\000\063\000\019\000\016\000\
\090\000\040\000\092\000\017\000\015\000\020\000\045\000\015\000\
\064\000\013\000\014\000\029\000\030\000\009\000\010\000\011\000\
\015\000\025\000\018\000\024\000\026\000\027\000\028\000\031\000\
\037\000\038\000\041\000\048\000\049\000\006\000\066\000\007\000\
\047\000\042\000\080\000\043\000\039\000\081\000\058\000\059\000\
\073\000\074\000\061\000\040\000\067\000\068\000\072\000\083\000\
\071\000\088\000\085\000\084\000\091\000\086\000\034\000\024\000\
\087\000\093\000\002\000\003\000\015\000\046\000\065\000\035\000\
\021\000\000\000\000\000\000\000\000\000\000\000\036\000"

let yycheck = "\064\000\
\003\001\003\001\040\000\068\000\001\001\001\000\002\001\045\000\
\005\001\047\000\003\001\003\001\003\001\058\000\059\000\018\001\
\019\001\019\001\014\001\084\000\016\001\086\000\067\000\016\001\
\016\001\016\001\071\000\003\001\009\001\004\001\003\001\002\001\
\084\000\014\001\086\000\006\001\013\001\010\001\014\001\016\001\
\015\001\007\000\008\000\022\000\023\000\003\001\003\001\008\001\
\002\001\009\001\003\001\007\001\015\001\003\001\000\000\007\001\
\011\001\002\001\017\001\003\001\003\001\000\000\002\001\000\000\
\012\001\016\001\002\001\016\001\009\001\002\001\017\001\017\001\
\003\001\003\001\016\001\014\001\014\001\011\001\013\001\002\001\
\017\001\002\001\004\001\015\001\003\001\014\001\011\001\002\001\
\016\001\016\001\008\001\008\001\016\001\036\000\052\000\011\001\
\016\001\255\255\255\255\255\255\255\255\255\255\016\001"

let yynames_const = "\
  EOF\000\
  IMPORT\000\
  SEMICOLON\000\
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
  BOOLEAN\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string list * blackbox list) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : notif_type list * clause list) in
    Obj.repr(
# 45 "parser.mly"
                                 ( match _1 with (imports, blackboxes) ->
        match _3 with (types, clauses) ->
        make_Program _2 imports blackboxes types clauses )
# 188 "../parser.ml"
               : program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 50 "parser.mly"
             ( ([_1], []) )
# 195 "../parser.ml"
               : string list * blackbox list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : blackbox) in
    Obj.repr(
# 51 "parser.mly"
               ( ([], [_1]) )
# 202 "../parser.ml"
               : string list * blackbox list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string list * blackbox list) in
    Obj.repr(
# 52 "parser.mly"
                 ( match _2 with (imports, blackboxes) -> (_1 :: imports, blackboxes) )
# 210 "../parser.ml"
               : string list * blackbox list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : blackbox) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string list * blackbox list) in
    Obj.repr(
# 53 "parser.mly"
                   ( match _2 with (imports, blackboxes) -> (imports, _1 :: blackboxes) )
# 218 "../parser.ml"
               : string list * blackbox list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : notif_type) in
    Obj.repr(
# 56 "parser.mly"
                ( ([_1], []) )
# 225 "../parser.ml"
               : notif_type list * clause list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : clause) in
    Obj.repr(
# 57 "parser.mly"
             ( ([], [_1]) )
# 232 "../parser.ml"
               : notif_type list * clause list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : notif_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : notif_type list * clause list) in
    Obj.repr(
# 58 "parser.mly"
                       ( match _2 with (types, clauses) -> (_1 :: types, clauses) )
# 240 "../parser.ml"
               : notif_type list * clause list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : clause) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : notif_type list * clause list) in
    Obj.repr(
# 59 "parser.mly"
                    ( match _2 with (types, clauses) -> (types, _1 :: clauses) )
# 248 "../parser.ml"
               : notif_type list * clause list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 62 "parser.mly"
                            ( _2 )
# 255 "../parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 65 "parser.mly"
                                                      ( make_External_BB _2 _4 (int_of_string _5) )
# 264 "../parser.ml"
               : blackbox))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 66 "parser.mly"
                              ( make_Internal_BB _2 )
# 271 "../parser.ml"
               : blackbox))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 69 "parser.mly"
                        ( _2 )
# 278 "../parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string list) in
    Obj.repr(
# 72 "parser.mly"
                                                       ( make_Type _2 _5 )
# 286 "../parser.ml"
               : notif_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 75 "parser.mly"
           ( [_1] )
# 293 "../parser.ml"
               : string list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string list) in
    Obj.repr(
# 76 "parser.mly"
                           ( _1 :: _3 )
# 301 "../parser.ml"
               : string list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : argument list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : literal list) in
    Obj.repr(
# 79 "parser.mly"
                                                                                 ( make_Plus_Minus_Clause _1 _3 _6 )
# 310 "../parser.ml"
               : clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : literal list) in
    Obj.repr(
# 80 "parser.mly"
                                                                       ( make_HelperClause _1 (List.map (fun str -> make_Arg_term(make_Variable(str))) _3) _6 )
# 319 "../parser.ml"
               : clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : literal list) in
    Obj.repr(
# 81 "parser.mly"
                                                             ( make_HelperClause _1 [] _5 )
# 327 "../parser.ml"
               : clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : argument) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : argument) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : literal list) in
    Obj.repr(
# 82 "parser.mly"
                                                                                 ( make_NotifClause _1 [_3; _4] _7 )
# 337 "../parser.ml"
               : clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : argument) in
    Obj.repr(
# 85 "parser.mly"
                ( [_1] )
# 344 "../parser.ml"
               : argument list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : argument) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string list) in
    Obj.repr(
# 86 "parser.mly"
                                ( _1 :: List.map (fun str -> make_Arg_term (make_Variable str)) _3 )
# 352 "../parser.ml"
               : argument list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "parser.mly"
                      ( make_Arg_notif (make_Notif_var _3 _1) )
# 360 "../parser.ml"
               : argument))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : literal) in
    Obj.repr(
# 92 "parser.mly"
              ( [_1] )
# 367 "../parser.ml"
               : literal list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : literal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : literal list) in
    Obj.repr(
# 93 "parser.mly"
                                 ( _1 :: _3 )
# 375 "../parser.ml"
               : literal list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : atom) in
    Obj.repr(
# 96 "parser.mly"
           ( Pos(_1) )
# 382 "../parser.ml"
               : literal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : atom) in
    Obj.repr(
# 97 "parser.mly"
               ( Neg(_2) )
# 389 "../parser.ml"
               : literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : term) in
    Obj.repr(
# 100 "parser.mly"
                       ( Equals(_1, _3) )
# 397 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : term list) in
    Obj.repr(
# 101 "parser.mly"
                                   ( make_Apply _1 _3 )
# 405 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 102 "parser.mly"
                         ( make_Apply _1 [] )
# 412 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : term list) in
    Obj.repr(
# 103 "parser.mly"
                                               ( make_Query _1 _3 _5 )
# 421 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 104 "parser.mly"
                                     ( make_Query _1 _3 [] )
# 429 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 105 "parser.mly"
              ( Bool(_1) )
# 436 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 108 "parser.mly"
           ( make_Constant_Variable _1 )
# 443 "../parser.ml"
               : term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "parser.mly"
                       ( make_Field_ref _1 _3 )
# 451 "../parser.ml"
               : term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : term) in
    Obj.repr(
# 112 "parser.mly"
           ( [_1] )
# 458 "../parser.ml"
               : term list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : term list) in
    Obj.repr(
# 113 "parser.mly"
                           ( _1 :: _3 )
# 466 "../parser.ml"
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
