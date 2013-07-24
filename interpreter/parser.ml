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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Types.Types;;
  open Type_Helpers;;
# 35 "../parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* IMPORT *);
  258 (* BLACKBOX *);
  259 (* MODULE *);
  260 (* TYPE *);
  261 (* PLUS *);
  262 (* MINUS *);
  263 (* HELPER *);
  264 (* ACTION *);
  265 (* NOT *);
  267 (* PERIOD *);
  268 (* AMPERSAND *);
  269 (* COLON_HYPHEN *);
  270 (* COLON *);
  271 (* SEMICOLON *);
  272 (* EQUALS *);
  273 (* LCURLY *);
  274 (* RCURLY *);
  275 (* COMMA *);
  276 (* LPAREN *);
  277 (* RPAREN *);
  278 (* DOUBLEQUOTE *);
    0|]

let yytransl_block = [|
  266 (* BOOLEAN *);
  279 (* DOTTED_IP *);
  280 (* NUMBER *);
  281 (* NAME *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\003\000\003\000\003\000\
\003\000\004\000\005\000\005\000\006\000\007\000\008\000\008\000\
\009\000\009\000\009\000\009\000\009\000\013\000\013\000\012\000\
\012\000\012\000\012\000\012\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\011\000\011\000\000\000"

let yylen = "\002\000\
\004\000\001\000\001\000\002\000\002\000\001\000\001\000\002\000\
\002\000\003\000\006\000\003\000\003\000\007\000\001\000\003\000\
\008\000\008\000\008\000\007\000\008\000\001\000\003\000\001\000\
\001\000\003\000\003\000\003\000\003\000\004\000\004\000\005\000\
\003\000\004\000\006\000\005\000\007\000\006\000\001\000\002\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\043\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\005\000\010\000\000\000\
\012\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\013\000\000\000\000\000\000\000\000\000\
\000\000\001\000\008\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\011\000\000\000\000\000\025\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\026\000\027\000\028\000\023\000\000\000\000\000\
\000\000\039\000\000\000\000\000\000\000\000\000\000\000\000\000\
\016\000\014\000\000\000\000\000\040\000\000\000\000\000\000\000\
\000\000\000\000\020\000\000\000\000\000\000\000\017\000\018\000\
\000\000\000\000\000\000\000\000\033\000\000\000\042\000\029\000\
\019\000\021\000\000\000\034\000\000\000\030\000\000\000\031\000\
\000\000\032\000\036\000\000\000\038\000\000\000\035\000\037\000"

let yydgoto = "\002\000\
\005\000\006\000\024\000\007\000\008\000\012\000\025\000\055\000\
\026\000\076\000\077\000\048\000\049\000"

let yysindex = "\005\000\
\003\255\000\000\242\254\009\255\000\000\025\255\003\255\003\255\
\029\255\017\255\030\255\046\255\000\000\000\000\000\000\049\255\
\000\000\054\255\038\255\051\255\056\255\060\255\065\255\091\000\
\046\255\046\255\068\255\000\000\077\255\074\255\075\255\076\255\
\078\255\000\000\000\000\000\000\082\255\083\255\062\255\062\255\
\035\255\062\255\000\000\079\255\080\255\000\000\032\255\084\255\
\081\255\085\255\086\255\087\255\088\255\091\255\089\255\090\255\
\092\255\093\255\062\255\098\255\100\255\000\255\101\255\102\255\
\079\255\104\255\000\000\000\000\000\000\000\000\000\255\000\255\
\013\255\000\000\016\255\097\255\105\255\106\255\000\255\000\255\
\000\000\000\000\108\255\109\255\000\000\028\255\110\255\096\255\
\040\255\000\255\000\000\062\255\112\255\113\255\000\000\000\000\
\107\255\045\255\062\255\111\255\000\000\114\255\000\000\000\000\
\000\000\000\000\116\255\000\000\117\255\000\000\053\255\000\000\
\058\255\000\000\000\000\118\255\000\000\119\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\122\255\126\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\101\000\130\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\026\255\120\255\
\000\000\000\000\000\000\000\000\000\000\115\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\121\255\127\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\121\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\128\255\000\000\000\000\000\000\000\000\
\000\000\000\000\128\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\010\000\063\000\000\000\000\000\000\000\000\000\069\000\
\000\000\000\000\192\255\197\255\216\255"

let yytablesize = 144
let yytable = "\050\000\
\052\000\053\000\078\000\003\000\004\000\001\000\083\000\084\000\
\073\000\074\000\009\000\078\000\078\000\087\000\093\000\094\000\
\013\000\014\000\070\000\078\000\078\000\045\000\085\000\046\000\
\075\000\103\000\088\000\011\000\016\000\058\000\078\000\017\000\
\104\000\010\000\045\000\089\000\046\000\086\000\097\000\110\000\
\024\000\058\000\057\000\015\000\024\000\058\000\024\000\098\000\
\102\000\019\000\020\000\021\000\022\000\023\000\018\000\051\000\
\045\000\109\000\046\000\047\000\101\000\045\000\029\000\046\000\
\047\000\108\000\045\000\028\000\046\000\047\000\116\000\027\000\
\118\000\115\000\045\000\030\000\046\000\047\000\117\000\045\000\
\031\000\046\000\047\000\045\000\032\000\046\000\047\000\035\000\
\036\000\033\000\034\000\037\000\038\000\039\000\040\000\041\000\
\043\000\042\000\062\000\044\000\006\000\060\000\059\000\054\000\
\056\000\061\000\066\000\063\000\064\000\065\000\071\000\067\000\
\072\000\079\000\080\000\090\000\068\000\069\000\082\000\091\000\
\100\000\092\000\095\000\096\000\002\000\099\000\105\000\106\000\
\003\000\007\000\111\000\107\000\015\000\081\000\112\000\113\000\
\024\000\114\000\119\000\120\000\022\000\041\000\000\000\027\000"

let yycheck = "\040\000\
\041\000\042\000\062\000\001\001\002\001\001\000\071\000\072\000\
\009\001\010\001\025\001\071\000\072\000\073\000\079\000\080\000\
\007\000\008\000\059\000\079\000\080\000\022\001\010\001\024\001\
\025\001\090\000\011\001\003\001\012\001\014\001\090\000\015\001\
\092\000\025\001\022\001\020\001\024\001\025\001\011\001\099\000\
\015\001\014\001\011\001\015\001\019\001\014\001\021\001\020\001\
\089\000\004\001\005\001\006\001\007\001\008\001\025\001\021\001\
\022\001\098\000\024\001\025\001\021\001\022\001\025\001\024\001\
\025\001\021\001\022\001\014\001\024\001\025\001\111\000\023\001\
\113\000\021\001\022\001\025\001\024\001\025\001\021\001\022\001\
\025\001\024\001\025\001\022\001\025\001\024\001\025\001\025\000\
\026\000\025\001\000\000\024\001\016\001\020\001\020\001\020\001\
\015\001\020\001\013\001\017\001\000\000\021\001\019\001\025\001\
\025\001\021\001\018\001\021\001\021\001\019\001\013\001\022\001\
\013\001\013\001\013\001\019\001\025\001\025\001\015\001\015\001\
\025\001\016\001\015\001\015\001\003\001\016\001\015\001\015\001\
\003\001\000\000\020\001\025\001\018\001\065\000\021\001\020\001\
\016\001\021\001\021\001\021\001\021\001\015\001\255\255\016\001"

let yynames_const = "\
  EOF\000\
  IMPORT\000\
  BLACKBOX\000\
  MODULE\000\
  TYPE\000\
  PLUS\000\
  MINUS\000\
  HELPER\000\
  ACTION\000\
  NOT\000\
  PERIOD\000\
  AMPERSAND\000\
  COLON_HYPHEN\000\
  COLON\000\
  SEMICOLON\000\
  EQUALS\000\
  LCURLY\000\
  RCURLY\000\
  COMMA\000\
  LPAREN\000\
  RPAREN\000\
  DOUBLEQUOTE\000\
  "

let yynames_block = "\
  BOOLEAN\000\
  DOTTED_IP\000\
  NUMBER\000\
  NAME\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string list * blackbox list) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : term_type list * clause list) in
    Obj.repr(
# 52 "parser.mly"
                                 ( match _1 with (imports, blackboxes) ->
        match _3 with (types, clauses) ->
        Parse_Helpers.process_program (Program(_2, imports, blackboxes, types, clauses)) )
# 227 "../parser.ml"
               : program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
             ( ([_1], []) )
# 234 "../parser.ml"
               : string list * blackbox list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : blackbox) in
    Obj.repr(
# 58 "parser.mly"
               ( ([], [_1]) )
# 241 "../parser.ml"
               : string list * blackbox list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string list * blackbox list) in
    Obj.repr(
# 59 "parser.mly"
                 ( match _2 with (imports, blackboxes) -> (_1 :: imports, blackboxes) )
# 249 "../parser.ml"
               : string list * blackbox list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : blackbox) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string list * blackbox list) in
    Obj.repr(
# 60 "parser.mly"
                   ( match _2 with (imports, blackboxes) -> (imports, _1 :: blackboxes) )
# 257 "../parser.ml"
               : string list * blackbox list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : term_type) in
    Obj.repr(
# 63 "parser.mly"
                ( ([_1], []) )
# 264 "../parser.ml"
               : term_type list * clause list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : clause) in
    Obj.repr(
# 64 "parser.mly"
             ( ([], [_1]) )
# 271 "../parser.ml"
               : term_type list * clause list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : term_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : term_type list * clause list) in
    Obj.repr(
# 65 "parser.mly"
                       ( match _2 with (types, clauses) -> (_1 :: types, clauses) )
# 279 "../parser.ml"
               : term_type list * clause list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : clause) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : term_type list * clause list) in
    Obj.repr(
# 66 "parser.mly"
                    ( match _2 with (types, clauses) -> (types, _1 :: clauses) )
# 287 "../parser.ml"
               : term_type list * clause list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 69 "parser.mly"
                            ( _2 )
# 294 "../parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 72 "parser.mly"
                                                         ( BlackBox(String.lowercase _2, External(_4, (int_of_string _5))) )
# 303 "../parser.ml"
               : blackbox))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 73 "parser.mly"
                              ( BlackBox(String.lowercase _2, Internal) )
# 310 "../parser.ml"
               : blackbox))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 76 "parser.mly"
                        ( String.lowercase _2 )
# 317 "../parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string list) in
    Obj.repr(
# 79 "parser.mly"
                                                       ( Type(String.lowercase _2, _5) )
# 325 "../parser.ml"
               : term_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 82 "parser.mly"
           ( [String.uppercase _1] )
# 332 "../parser.ml"
               : string list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string list) in
    Obj.repr(
# 83 "parser.mly"
                           ( (String.uppercase _1) :: _3 )
# 340 "../parser.ml"
               : string list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : term list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : atom list) in
    Obj.repr(
# 86 "parser.mly"
                                                                         ( Clause(Signature(Plus, String.lowercase _2, _4), _7) )
# 349 "../parser.ml"
               : clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : term list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : atom list) in
    Obj.repr(
# 87 "parser.mly"
                                                                          ( Clause(Signature(Minus, String.lowercase _2, _4), _7) )
# 358 "../parser.ml"
               : clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : term list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : atom list) in
    Obj.repr(
# 88 "parser.mly"
                                                                           ( Clause(Signature(Helper, String.lowercase _2, _4), _7) )
# 367 "../parser.ml"
               : clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : atom list) in
    Obj.repr(
# 89 "parser.mly"
                                                                 ( Clause(Signature(Helper, String.lowercase _2, []), _6) )
# 375 "../parser.ml"
               : clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : term list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : atom list) in
    Obj.repr(
# 90 "parser.mly"
                                                                           ( Clause(Signature(Action, String.lowercase _2, _4) ,_7) )
# 384 "../parser.ml"
               : clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : term) in
    Obj.repr(
# 93 "parser.mly"
           ( [_1] )
# 391 "../parser.ml"
               : term list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : term list) in
    Obj.repr(
# 94 "parser.mly"
                           ( _1 :: _3 )
# 399 "../parser.ml"
               : term list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "parser.mly"
           ( Variable(String.uppercase _1, raw_type) )
# 406 "../parser.ml"
               : term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 98 "parser.mly"
             ( Constant([_1], raw_type) )
# 413 "../parser.ml"
               : term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 99 "parser.mly"
                                   ( Constant([_2], raw_type) (* WHAT IF THERE ARE SPACES? use String.map (fun c -> if c = ' ' then '_' else c) maybe? *))
# 420 "../parser.ml"
               : term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 100 "parser.mly"
                       ( Field_ref(String.uppercase _1, String.uppercase _3) )
# 428 "../parser.ml"
               : term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 101 "parser.mly"
                      ( Variable(String.uppercase _1, Term_defer(String.lowercase _3)) )
# 436 "../parser.ml"
               : term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : term) in
    Obj.repr(
# 104 "parser.mly"
                       ( Equals(true, _1, _3) )
# 444 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : term) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : term) in
    Obj.repr(
# 105 "parser.mly"
                           ( Equals(false, _2, _2) )
# 452 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : term list) in
    Obj.repr(
# 106 "parser.mly"
                                   ( Apply(true, String.lowercase _1, _3) )
# 460 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : term list) in
    Obj.repr(
# 107 "parser.mly"
                                       ( Apply(false, String.lowercase _2, _4) )
# 468 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 108 "parser.mly"
                         ( Apply(true, String.lowercase _1, []) )
# 475 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 109 "parser.mly"
                             ( Apply(false, String.lowercase _2, []) )
# 482 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : term list) in
    Obj.repr(
# 110 "parser.mly"
                                               ( Apply(true, (String.lowercase _1) ^ "/" ^ (String.lowercase _3), _5) )
# 491 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 111 "parser.mly"
                                     ( Apply(false, (String.lowercase _1) ^ "/" ^ (String.lowercase _3), []) )
# 499 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : term list) in
    Obj.repr(
# 112 "parser.mly"
                                                   ( Apply(false, (String.lowercase _2) ^ "/" ^ (String.lowercase _4), _6) )
# 508 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 113 "parser.mly"
                                         ( Apply(false, (String.lowercase _2) ^ "/" ^ (String.lowercase _4), []) )
# 516 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 114 "parser.mly"
              ( Bool(_1) )
# 523 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 115 "parser.mly"
                  ( Bool(not _2) )
# 530 "../parser.ml"
               : atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : atom) in
    Obj.repr(
# 118 "parser.mly"
           ( [_1] )
# 537 "../parser.ml"
               : atom list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : atom list) in
    Obj.repr(
# 119 "parser.mly"
                           ( _1 :: _3 )
# 545 "../parser.ml"
               : atom list))
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
