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

open Parsing;;
let _ = parse_error;;
# 2 "Surface_Parser.mly"
  open Flowlog_Types;;
# 51 "../Surface_Parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* IMPORT *);
  258 (* TABLE *);
  259 (* REMOTE *);
  260 (* OUTGOING *);
  261 (* THEN *);
  262 (* INCOMING *);
  263 (* DO *);
  264 (* AT *);
  265 (* TIMEOUT *);
  266 (* PURE *);
  267 (* ON *);
  268 (* SEND *);
  269 (* TO *);
  270 (* COLON_EQUALS *);
  271 (* DELETE *);
  272 (* INSERT *);
  273 (* WHERE *);
  274 (* EVENT *);
  275 (* INTO *);
  276 (* FROM *);
  277 (* NOT *);
  278 (* IMPLIES *);
  279 (* TRUE *);
  280 (* FALSE *);
  281 (* IFF *);
  282 (* OR *);
  283 (* AND *);
  284 (* PERIOD *);
  285 (* COLON *);
  286 (* SEMICOLON *);
  287 (* EQUALS *);
  288 (* NOTEQUALS *);
  289 (* LCURLY *);
  290 (* RCURLY *);
  291 (* COMMA *);
  292 (* LPAREN *);
  293 (* RPAREN *);
  294 (* DOUBLEQUOTE *);
    0|]

let yytransl_block = [|
  295 (* BOOLEAN *);
  296 (* DOTTED_IP *);
  297 (* NUMBER *);
  298 (* NAME *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\006\000\007\000\007\000\007\000\009\000\009\000\
\009\000\009\000\009\000\008\000\008\000\008\000\013\000\011\000\
\011\000\011\000\010\000\014\000\016\000\016\000\016\000\017\000\
\017\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
\018\000\018\000\018\000\018\000\018\000\019\000\019\000\019\000\
\019\000\002\000\002\000\003\000\003\000\015\000\015\000\012\000\
\012\000\012\000\005\000\005\000\004\000\004\000\000\000"

let yylen = "\002\000\
\003\000\002\000\003\000\001\000\001\000\001\000\006\000\007\000\
\006\000\006\000\006\000\010\000\016\000\007\000\003\000\003\000\
\001\000\000\000\003\000\005\000\008\000\008\000\007\000\002\000\
\000\000\001\000\001\000\003\000\003\000\004\000\008\000\002\000\
\003\000\003\000\003\000\003\000\003\000\001\000\001\000\003\000\
\003\000\001\000\003\000\001\000\003\000\001\000\002\000\001\000\
\003\000\000\000\001\000\002\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\055\000\000\000\000\000\000\000\000\000\004\000\005\000\
\006\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\054\000\052\000\000\000\003\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\019\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\047\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\020\000\000\000\000\000\000\000\039\000\000\000\000\000\000\000\
\000\000\045\000\007\000\000\000\000\000\000\000\011\000\000\000\
\010\000\009\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\008\000\000\000\014\000\000\000\040\000\041\000\000\000\
\043\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\017\000\000\000\000\000\000\000\026\000\027\000\000\000\
\000\000\000\000\000\000\023\000\000\000\000\000\000\000\012\000\
\000\000\032\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\021\000\022\000\016\000\000\000\000\000\
\000\000\035\000\000\000\000\000\000\000\000\000\000\000\033\000\
\028\000\029\000\000\000\000\000\000\000\000\000\030\000\015\000\
\000\000\049\000\000\000\000\000\000\000\000\000\000\000\013\000\
\031\000"

let yydgoto = "\002\000\
\010\000\071\000\046\000\011\000\012\000\013\000\014\000\015\000\
\016\000\017\000\107\000\136\000\137\000\018\000\043\000\044\000\
\102\000\114\000\115\000"

let yysindex = "\015\000\
\053\255\000\000\242\254\251\254\037\255\018\255\023\255\032\255\
\034\255\000\000\064\255\041\000\085\255\064\255\000\000\000\000\
\000\000\069\255\073\255\057\255\079\255\092\255\255\254\093\255\
\083\255\118\000\000\000\000\000\000\000\003\255\000\000\084\255\
\249\254\084\255\114\255\089\255\091\255\084\255\000\000\094\255\
\096\255\098\255\000\000\003\255\100\255\101\255\095\255\084\255\
\102\255\121\255\104\255\105\255\109\255\108\255\081\255\081\255\
\000\000\084\255\115\255\138\255\110\255\000\255\106\255\119\255\
\000\000\120\255\081\255\111\255\000\000\123\255\117\255\122\255\
\118\255\000\000\000\000\112\255\126\255\146\255\000\000\129\255\
\000\000\000\000\124\255\125\255\127\255\140\255\081\255\143\255\
\130\255\000\000\147\255\000\000\149\255\000\000\000\000\128\255\
\000\000\131\255\253\254\132\255\002\255\134\255\149\255\149\255\
\135\255\000\000\137\255\139\255\002\255\000\000\000\000\002\255\
\237\254\020\255\017\255\000\000\145\255\148\255\141\255\000\000\
\142\255\000\000\036\255\144\255\081\255\002\255\002\255\002\255\
\002\255\081\255\081\255\000\000\000\000\000\000\154\255\151\255\
\152\255\000\000\153\255\155\255\065\255\063\255\150\255\000\000\
\000\000\000\000\156\255\166\255\142\255\081\255\000\000\000\000\
\157\255\000\000\158\255\159\255\169\255\160\255\161\255\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\077\255\181\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\243\254\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\074\255\000\000\162\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\163\255\000\000\000\000\000\000\
\000\000\000\000\164\255\000\000\000\000\000\000\163\255\163\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\076\255\171\255\000\000\000\000\000\000\000\000\000\000\000\000\
\168\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\170\255\000\000\082\255\000\000\090\255\246\254\080\255\000\000\
\000\000\000\000\000\000\000\000\168\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\200\255\230\255\169\000\073\000\000\000\000\000\000\000\
\000\000\000\000\000\000\039\000\000\000\000\000\147\000\000\000\
\021\000\180\255\203\255"

let yytablesize = 275
let yytable = "\073\000\
\046\000\072\000\072\000\035\000\078\000\105\000\106\000\049\000\
\124\000\040\000\083\000\053\000\047\000\072\000\037\000\001\000\
\125\000\041\000\042\000\037\000\044\000\061\000\109\000\044\000\
\110\000\111\000\037\000\019\000\048\000\079\000\097\000\074\000\
\122\000\072\000\036\000\123\000\020\000\112\000\021\000\068\000\
\027\000\126\000\069\000\113\000\127\000\128\000\129\000\130\000\
\131\000\141\000\142\000\143\000\144\000\003\000\004\000\005\000\
\006\000\126\000\007\000\022\000\127\000\128\000\129\000\008\000\
\023\000\004\000\005\000\006\000\140\000\007\000\009\000\072\000\
\138\000\024\000\008\000\025\000\145\000\146\000\053\000\053\000\
\053\000\009\000\053\000\026\000\126\000\003\000\029\000\053\000\
\128\000\129\000\128\000\129\000\032\000\155\000\053\000\038\000\
\072\000\030\000\038\000\038\000\038\000\034\000\031\000\038\000\
\034\000\034\000\038\000\038\000\038\000\034\000\038\000\036\000\
\041\000\041\000\036\000\038\000\034\000\039\000\068\000\036\000\
\033\000\069\000\070\000\117\000\118\000\045\000\036\000\034\000\
\037\000\050\000\051\000\055\000\052\000\056\000\058\000\054\000\
\060\000\059\000\062\000\063\000\064\000\065\000\066\000\067\000\
\075\000\076\000\077\000\080\000\081\000\082\000\085\000\089\000\
\084\000\086\000\088\000\090\000\087\000\091\000\092\000\096\000\
\093\000\098\000\094\000\116\000\100\000\101\000\120\000\147\000\
\095\000\103\000\099\000\121\000\104\000\108\000\132\000\119\000\
\129\000\133\000\153\000\159\000\051\000\028\000\134\000\135\000\
\148\000\139\000\149\000\154\000\150\000\160\000\057\000\151\000\
\025\000\018\000\157\000\000\000\156\000\152\000\042\000\158\000\
\024\000\050\000\161\000\048\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\046\000\046\000\046\000\000\000\046\000\000\000\
\000\000\000\000\000\000\046\000\000\000\000\000\000\000\000\000\
\000\000\000\000\046\000"

let yycheck = "\056\000\
\000\000\055\000\056\000\005\001\005\001\009\001\010\001\034\000\
\028\001\007\001\067\000\038\000\020\001\067\000\025\001\001\000\
\036\001\015\001\016\001\030\001\034\001\048\000\021\001\037\001\
\023\001\024\001\037\001\042\001\036\001\030\001\087\000\058\000\
\109\000\087\000\036\001\112\000\042\001\036\001\002\001\038\001\
\000\000\022\001\041\001\042\001\025\001\026\001\027\001\031\001\
\032\001\126\000\127\000\128\000\129\000\001\001\002\001\003\001\
\004\001\022\001\006\001\042\001\025\001\026\001\027\001\011\001\
\042\001\002\001\003\001\004\001\125\000\006\001\018\001\125\000\
\037\001\042\001\011\001\042\001\130\000\131\000\002\001\003\001\
\004\001\018\001\006\001\011\000\022\001\001\001\014\000\011\001\
\026\001\027\001\026\001\027\001\036\001\150\000\018\001\022\001\
\150\000\029\001\025\001\026\001\027\001\022\001\030\001\030\001\
\025\001\026\001\031\001\032\001\035\001\030\001\037\001\022\001\
\031\001\032\001\025\001\033\001\037\001\000\000\038\001\030\001\
\042\001\041\001\042\001\103\000\104\000\042\001\037\001\036\001\
\036\001\016\001\042\001\036\001\042\001\036\001\035\001\042\001\
\042\001\037\001\037\001\019\001\037\001\037\001\034\001\036\001\
\030\001\008\001\037\001\042\001\030\001\030\001\028\001\040\001\
\042\001\037\001\037\001\030\001\035\001\012\001\030\001\020\001\
\037\001\019\001\038\001\030\001\018\001\017\001\030\001\014\001\
\042\001\042\001\041\001\033\001\042\001\042\001\030\001\041\001\
\027\001\030\001\013\001\011\001\000\000\013\000\042\001\042\001\
\034\001\042\001\035\001\149\000\036\001\030\001\044\000\037\001\
\030\001\030\001\037\001\255\255\040\001\042\001\037\001\041\001\
\030\001\034\001\042\001\034\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\004\001\255\255\006\001\255\255\
\255\255\255\255\255\255\011\001\255\255\255\255\255\255\255\255\
\255\255\255\255\018\001"

let yynames_const = "\
  EOF\000\
  IMPORT\000\
  TABLE\000\
  REMOTE\000\
  OUTGOING\000\
  THEN\000\
  INCOMING\000\
  DO\000\
  AT\000\
  TIMEOUT\000\
  PURE\000\
  ON\000\
  SEND\000\
  TO\000\
  COLON_EQUALS\000\
  DELETE\000\
  INSERT\000\
  WHERE\000\
  EVENT\000\
  INTO\000\
  FROM\000\
  NOT\000\
  IMPLIES\000\
  TRUE\000\
  FALSE\000\
  IFF\000\
  OR\000\
  AND\000\
  PERIOD\000\
  COLON\000\
  SEMICOLON\000\
  EQUALS\000\
  NOTEQUALS\000\
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
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'import_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 68 "Surface_Parser.mly"
                                        (AST(_1, _2))
# 330 "../Surface_Parser.ml"
               : flowlog_ast))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 69 "Surface_Parser.mly"
                            (AST([], _1))
# 337 "../Surface_Parser.ml"
               : flowlog_ast))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 71 "Surface_Parser.mly"
                                (_2)
# 344 "../Surface_Parser.ml"
               : 'import))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'reactive_stmt) in
    Obj.repr(
# 74 "Surface_Parser.mly"
                            ([SReactive(_1)])
# 351 "../Surface_Parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decl_stmt) in
    Obj.repr(
# 75 "Surface_Parser.mly"
                        ([SDecl(_1)])
# 358 "../Surface_Parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rule_stmt) in
    Obj.repr(
# 76 "Surface_Parser.mly"
                        (List.map (fun r -> SRule(r)) _1)
# 365 "../Surface_Parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string list) in
    Obj.repr(
# 79 "Surface_Parser.mly"
                                                           (DeclTable(_2, _4))
# 373 "../Surface_Parser.ml"
               : 'decl_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string list) in
    Obj.repr(
# 80 "Surface_Parser.mly"
                                                                  (DeclRemoteTable(_3, _5))
# 381 "../Surface_Parser.ml"
               : 'decl_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string list) in
    Obj.repr(
# 81 "Surface_Parser.mly"
                                                           (DeclEvent(_2, _4))
# 389 "../Surface_Parser.ml"
               : 'decl_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 82 "Surface_Parser.mly"
                                                         (DeclInc(_2, _4))
# 397 "../Surface_Parser.ml"
               : 'decl_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string list) in
    Obj.repr(
# 83 "Surface_Parser.mly"
                                                              (DeclOut(_2, _4))
# 405 "../Surface_Parser.ml"
               : 'decl_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'refresh_clause) in
    Obj.repr(
# 90 "Surface_Parser.mly"
              (ReactRemote(_3, _5, _7, _8, _9))
# 416 "../Surface_Parser.ml"
               : 'reactive_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 14 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 12 : string list) in
    let _9 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _11 = (Parsing.peek_val __caml_parser_env 5 : 'possempty_assign_list) in
    let _14 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _15 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 93 "Surface_Parser.mly"
              (ReactOut(_2, _4, _9, _11, OutSend(_14, _15)))
# 428 "../Surface_Parser.ml"
               : 'reactive_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 95 "Surface_Parser.mly"
              (ReactInc(_2, _6))
# 436 "../Surface_Parser.ml"
               : 'reactive_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "Surface_Parser.mly"
                                 ({afield=_1; atupvar=_3})
# 444 "../Surface_Parser.ml"
               : 'assign))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 100 "Surface_Parser.mly"
                                  (RefreshTimeout(int_of_string(_2), _3))
# 452 "../Surface_Parser.ml"
               : 'refresh_clause))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "Surface_Parser.mly"
                   (RefreshPure)
# 458 "../Surface_Parser.ml"
               : 'refresh_clause))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "Surface_Parser.mly"
              (RefreshEvery)
# 464 "../Surface_Parser.ml"
               : 'refresh_clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'on_clause) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'action_clause_list) in
    Obj.repr(
# 105 "Surface_Parser.mly"
    (List.map (fun act -> Rule(List.hd _1, List.hd (List.tl _1), act)) _3)
# 472 "../Surface_Parser.ml"
               : 'rule_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 107 "Surface_Parser.mly"
                                        ([_2;_4])
# 480 "../Surface_Parser.ml"
               : 'on_clause))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : term list) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'optional_fmla) in
    Obj.repr(
# 110 "Surface_Parser.mly"
                                                                               (ADelete(_6, _3, _7))
# 489 "../Surface_Parser.ml"
               : 'action_clause))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : term list) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'optional_fmla) in
    Obj.repr(
# 111 "Surface_Parser.mly"
                                                                               (AInsert(_6, _3, _7))
# 498 "../Surface_Parser.ml"
               : 'action_clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : term list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'optional_fmla) in
    Obj.repr(
# 112 "Surface_Parser.mly"
                                                                      (ADo(_2, _4, _6))
# 507 "../Surface_Parser.ml"
               : 'action_clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 115 "Surface_Parser.mly"
                            (_2)
# 514 "../Surface_Parser.ml"
               : 'optional_fmla))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "Surface_Parser.mly"
              (FTrue)
# 520 "../Surface_Parser.ml"
               : 'optional_fmla))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "Surface_Parser.mly"
                   (FTrue)
# 526 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "Surface_Parser.mly"
                    (FFalse)
# 532 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 121 "Surface_Parser.mly"
                               (FEquals(_1, _3))
# 540 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 122 "Surface_Parser.mly"
                                  (FNot(FEquals(_1, _3)))
# 548 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : term list) in
    Obj.repr(
# 123 "Surface_Parser.mly"
                                           (FAtom("", _1, _3))
# 556 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : term list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 124 "Surface_Parser.mly"
                                                               (FAtom(_1, _3, _5))
# 566 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 125 "Surface_Parser.mly"
                          (FNot(_2))
# 573 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 126 "Surface_Parser.mly"
                                  (FAnd(_1, _3))
# 581 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 127 "Surface_Parser.mly"
                                 (FOr(_1, _3))
# 589 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 128 "Surface_Parser.mly"
                                    (_2)
# 596 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 129 "Surface_Parser.mly"
                                      (FOr(FNot(_1), _3))
# 604 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 130 "Surface_Parser.mly"
                                  (FOr(FAnd(_1, _3), FAnd(FNot(_1), FNot(_3))))
# 612 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 132 "Surface_Parser.mly"
                   (TVar(_1))
# 619 "../Surface_Parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 133 "Surface_Parser.mly"
                     (TConst(_1))
# 626 "../Surface_Parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 134 "Surface_Parser.mly"
                                           (TConst(_2))
# 633 "../Surface_Parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 135 "Surface_Parser.mly"
                               (TField(_1, _3))
# 641 "../Surface_Parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 138 "Surface_Parser.mly"
                   ([_1])
# 648 "../Surface_Parser.ml"
               : term list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : term list) in
    Obj.repr(
# 139 "Surface_Parser.mly"
                                   (_1 :: _3)
# 656 "../Surface_Parser.ml"
               : term list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 142 "Surface_Parser.mly"
                   ([_1])
# 663 "../Surface_Parser.ml"
               : string list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string list) in
    Obj.repr(
# 143 "Surface_Parser.mly"
                                   (_1 :: _3)
# 671 "../Surface_Parser.ml"
               : string list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'action_clause) in
    Obj.repr(
# 146 "Surface_Parser.mly"
                            ([_1])
# 678 "../Surface_Parser.ml"
               : 'action_clause_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'action_clause) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'action_clause_list) in
    Obj.repr(
# 147 "Surface_Parser.mly"
                                               (_1 :: _2)
# 686 "../Surface_Parser.ml"
               : 'action_clause_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assign) in
    Obj.repr(
# 150 "Surface_Parser.mly"
                     ([_1])
# 693 "../Surface_Parser.ml"
               : 'possempty_assign_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assign) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'possempty_assign_list) in
    Obj.repr(
# 151 "Surface_Parser.mly"
                                                 (_1 :: _3)
# 701 "../Surface_Parser.ml"
               : 'possempty_assign_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 152 "Surface_Parser.mly"
              ([])
# 707 "../Surface_Parser.ml"
               : 'possempty_assign_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 155 "Surface_Parser.mly"
                   (_1)
# 714 "../Surface_Parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 156 "Surface_Parser.mly"
                             (_1 @ _2)
# 722 "../Surface_Parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'import) in
    Obj.repr(
# 159 "Surface_Parser.mly"
                     ([_1])
# 729 "../Surface_Parser.ml"
               : 'import_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'import) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'import_list) in
    Obj.repr(
# 160 "Surface_Parser.mly"
                                 (_1 :: _2)
# 737 "../Surface_Parser.ml"
               : 'import_list))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : flowlog_ast)
