type token =
  | EOF
  | IMPORT
  | SEMICOLON
  | NAME of (string)
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
# 30 "../parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* IMPORT *);
  258 (* SEMICOLON *);
  260 (* PERIOD *);
  261 (* BLACKBOX *);
  262 (* AMPERSAND *);
  265 (* MODULE *);
  266 (* COLON *);
  267 (* TYPE *);
  268 (* EQUALS *);
  269 (* LCURLY *);
  270 (* RCURLY *);
  271 (* COMMA *);
  272 (* LPAREN *);
  273 (* RPAREN *);
  274 (* COLON_HYPHEN *);
  275 (* NOT *);
    0|]

let yytransl_block = [|
  259 (* NAME *);
  263 (* NUMBER *);
  264 (* DOTTED_IP *);
  276 (* BOOLEAN *);
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
\007\000\007\000\006\000\009\000\001\000\003\000\003\000\001\000\
\003\000\001\000\002\000\003\000\004\000\003\000\006\000\005\000\
\001\000\001\000\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\038\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\005\000\010\000\012\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\000\000\000\000\001\000\008\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\011\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\023\000\000\000\016\000\
\000\000\000\000\033\000\000\000\000\000\026\000\000\000\000\000\
\000\000\022\000\000\000\000\000\000\000\000\000\027\000\019\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\030\000\000\000\000\000\025\000\028\000\018\000\017\000\000\000\
\014\000\000\000\000\000\000\000\029\000\000\000\032\000\000\000\
\035\000\037\000\020\000\031\000"

let yydgoto = "\002\000\
\005\000\006\000\021\000\007\000\008\000\012\000\022\000\034\000\
\023\000\035\000\036\000\052\000\053\000\054\000\055\000\075\000"

let yysindex = "\035\000\
\030\255\000\000\041\255\042\255\000\000\037\255\030\255\030\255\
\045\255\031\255\046\255\027\255\000\000\000\000\000\000\000\000\
\040\255\043\255\034\255\048\255\052\000\027\255\027\255\047\255\
\000\000\003\255\044\255\000\000\000\000\000\000\053\255\251\254\
\039\255\049\255\050\255\054\255\051\255\000\000\055\255\056\255\
\255\254\052\255\057\255\058\255\056\255\000\000\059\255\000\000\
\023\255\001\255\000\000\060\255\061\255\000\000\065\255\255\254\
\255\254\000\000\062\255\064\255\068\255\005\255\000\000\000\000\
\255\254\069\255\063\255\066\255\067\255\071\255\070\255\076\255\
\000\000\072\255\073\255\000\000\000\000\000\000\000\000\255\254\
\000\000\012\255\078\255\069\255\000\000\080\255\000\000\074\255\
\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\075\255\079\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\060\000\063\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\077\255\
\000\000\000\000\000\000\081\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\253\254\000\000\
\083\255\000\000\000\000\000\000\087\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\084\255\011\255\
\000\000\082\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\033\000\020\000\000\000\000\000\000\000\000\000\228\255\
\000\000\000\000\039\000\200\255\000\000\042\000\197\255\206\255"

let yytablesize = 99
let yytable = "\067\000\
\068\000\049\000\074\000\049\000\039\000\032\000\077\000\072\000\
\076\000\040\000\015\000\048\000\034\000\015\000\072\000\058\000\
\060\000\050\000\051\000\033\000\051\000\073\000\074\000\086\000\
\074\000\034\000\061\000\034\000\087\000\019\000\003\000\088\000\
\016\000\090\000\004\000\001\000\017\000\020\000\062\000\013\000\
\014\000\029\000\030\000\009\000\010\000\011\000\015\000\024\000\
\018\000\026\000\027\000\028\000\025\000\031\000\038\000\037\000\
\041\000\046\000\047\000\006\000\032\000\064\000\007\000\045\000\
\078\000\042\000\043\000\079\000\044\000\056\000\071\000\072\000\
\081\000\040\000\057\000\065\000\066\000\070\000\069\000\083\000\
\089\000\091\000\059\000\002\000\080\000\082\000\084\000\003\000\
\024\000\085\000\092\000\063\000\000\000\015\000\034\000\035\000\
\000\000\021\000\036\000"

let yycheck = "\056\000\
\057\000\003\001\062\000\003\001\010\001\003\001\066\000\003\001\
\065\000\015\001\014\001\040\000\002\001\017\001\003\001\044\000\
\045\000\019\001\020\001\017\001\020\001\017\001\082\000\080\000\
\084\000\015\001\004\001\017\001\017\001\003\001\001\001\082\000\
\002\001\084\000\005\001\001\000\006\001\011\001\016\001\007\000\
\008\000\022\000\023\000\003\001\003\001\009\001\002\001\008\001\
\003\001\016\001\003\001\000\000\010\001\007\001\002\001\012\001\
\018\001\003\001\003\001\000\000\003\001\002\001\000\000\013\001\
\002\001\017\001\017\001\002\001\015\001\018\001\003\001\003\001\
\002\001\015\001\018\001\015\001\012\001\014\001\017\001\004\001\
\003\001\002\001\044\000\009\001\018\001\016\001\015\001\009\001\
\002\001\017\001\017\001\050\000\255\255\017\001\012\001\012\001\
\255\255\017\001\017\001"

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
# 46 "parser.mly"
                                 ( match _1 with (imports, blackboxes) ->
        match _3 with (types, clauses) ->
        make_Program _2 imports blackboxes types clauses )
# 191 "../parser.ml"
               : program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 51 "parser.mly"
             ( ([_1], []) )
# 198 "../parser.ml"
               : string list * blackbox list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : blackbox) in
    Obj.repr(
# 52 "parser.mly"
               ( ([], [_1]) )
# 205 "../parser.ml"
               : string list * blackbox list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string list * blackbox list) in
    Obj.repr(
# 53 "parser.mly"
                 ( match _2 with (imports, blackboxes) -> (_1 :: imports, blackboxes) )
# 213 "../parser.ml"
               : string list * blackbox list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : blackbox) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string list * blackbox list) in
    Obj.repr(
# 54 "parser.mly"
                   ( match _2 with (imports, blackboxes) -> (imports, _1 :: blackboxes) )
# 221 "../parser.ml"
               : string list * blackbox list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : notif_type) in
    Obj.repr(
# 57 "parser.mly"
                ( ([_1], []) )
# 228 "../parser.ml"
               : notif_type list * clause list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : clause) in
    Obj.repr(
# 58 "parser.mly"
             ( ([], [_1]) )
# 235 "../parser.ml"
               : notif_type list * clause list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : notif_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : notif_type list * clause list) in
    Obj.repr(
# 59 "parser.mly"
                       ( match _2 with (types, clauses) -> (_1 :: types, clauses) )
# 243 "../parser.ml"
               : notif_type list * clause list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : clause) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : notif_type list * clause list) in
    Obj.repr(
# 60 "parser.mly"
                    ( match _2 with (types, clauses) -> (types, _1 :: clauses) )
# 251 "../parser.ml"
               : notif_type list * clause list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 63 "parser.mly"
                            ( _2 )
# 258 "../parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 66 "parser.mly"
                                                         ( make_External_BB _2 _4 (int_of_string _5) )
# 267 "../parser.ml"
               : blackbox))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 67 "parser.mly"
                              ( make_Internal_BB _2 )
# 274 "../parser.ml"
               : blackbox))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 70 "parser.mly"
                        ( _2 )
# 281 "../parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string list) in
    Obj.repr(
# 73 "parser.mly"
                                                       ( make_Type _2 _5 )
# 289 "../parser.ml"
               : notif_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 76 "parser.mly"
           ( [String.uppercase _1] )
# 296 "../parser.ml"
               : string list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string list) in
    Obj.repr(
# 77 "parser.mly"
                           ( (String.uppercase _1) :: _3 )
# 304 "../parser.ml"
               : string list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : argument list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : literal list) in
    Obj.repr(
# 80 "parser.mly"
                                                                                 ( make_Plus_Minus_Clause _1 _3 _6 )
# 313 "../parser.ml"
               : clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : literal list) in
    Obj.repr(
# 81 "parser.mly"
                                                                       ( make_HelperClause _1 (List.map (fun str -> make_Arg_term(make_Variable(str))) _3) _6 )
# 322 "../parser.ml"
               : clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : literal list) in
    Obj.repr(
# 82 "parser.mly"
                                                             ( make_HelperClause _1 [] _5 )
# 330 "../parser.ml"
               : clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : argument) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : argument) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : literal list) in
    Obj.repr(
# 83 "parser.mly"
                                                                                       ( make_NotifClause _1 [_3; _5] _8 )
# 340 "../parser.ml"
               : clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : argument) in
    Obj.repr(
# 86 "parser.mly"
                ( [_1] )
# 347 "../parser.ml"
               : argument list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : argument) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string list) in
    Obj.repr(
# 87 "parser.mly"
                                ( _1 :: List.map (fun str -> make_Arg_term (make_Variable str)) _3 )
# 355 "../parser.ml"
               : argument list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "parser.mly"
                      ( make_Arg_notif (make_Notif_var _3 (String.uppercase _1)) )
# 363 "../parser.ml"
               : argument))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : literal) in
    Obj.repr(
# 93 "parser.mly"
              ( [_1] )
# 370 "../parser.ml"
               : literal list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : literal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : literal list) in
    Obj.repr(
# 94 "parser.mly"
                                 ( _1 :: _3 )
# 378 "../parser.ml"
               : literal list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : atom) in
    Obj.repr(
# 97 "parser.mly"
           ( Pos(_1) )
# 385 "../parser.ml"
               : literal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : atom) in
    Obj.repr(
# 98 "parser.mly"
               ( Neg(_2) )
# 392 "../parser.ml"
               : literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : term) in
    Obj.repr(
# 101 "parser.mly"
                       ( Equals(_1, _3) )
# 400 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : term list) in
    Obj.repr(
# 102 "parser.mly"
                                   ( make_Apply _1 _3 )
# 408 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 103 "parser.mly"
                         ( make_Apply _1 [] )
# 415 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : term list) in
    Obj.repr(
# 104 "parser.mly"
                                               ( make_Query _1 _3 _5 )
# 424 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 105 "parser.mly"
                                     ( make_Query _1 _3 [] )
# 432 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 106 "parser.mly"
              ( Bool(_1) )
# 439 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "parser.mly"
           ( make_Constant_Variable (String.uppercase _1) )
# 446 "../parser.ml"
               : term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 110 "parser.mly"
                       ( make_Field_ref (String.uppercase _1) (String.uppercase _3) )
# 454 "../parser.ml"
               : term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : term) in
    Obj.repr(
# 113 "parser.mly"
           ( [_1] )
# 461 "../parser.ml"
               : term list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : term list) in
    Obj.repr(
# 114 "parser.mly"
                           ( _1 :: _3 )
# 469 "../parser.ml"
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
