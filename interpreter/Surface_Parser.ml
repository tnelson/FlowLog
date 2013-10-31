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
  open Flowlog_Types
  open ExtList.List
# 52 "../Surface_Parser.ml"
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
\009\000\009\000\009\000\011\000\011\000\008\000\008\000\008\000\
\014\000\012\000\012\000\012\000\010\000\015\000\018\000\018\000\
\018\000\017\000\017\000\019\000\019\000\019\000\019\000\019\000\
\019\000\019\000\019\000\019\000\019\000\019\000\019\000\020\000\
\020\000\020\000\020\000\002\000\002\000\003\000\003\000\016\000\
\016\000\013\000\013\000\013\000\005\000\005\000\004\000\004\000\
\000\000"

let yylen = "\002\000\
\003\000\002\000\003\000\001\000\001\000\001\000\006\000\007\000\
\006\000\006\000\006\000\001\000\000\000\011\000\017\000\007\000\
\003\000\003\000\001\000\000\000\003\000\006\000\008\000\008\000\
\007\000\002\000\000\000\001\000\001\000\003\000\003\000\004\000\
\008\000\002\000\003\000\003\000\003\000\003\000\003\000\001\000\
\001\000\003\000\003\000\001\000\003\000\001\000\003\000\001\000\
\002\000\001\000\003\000\000\000\001\000\002\000\001\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\057\000\000\000\000\000\000\000\000\000\004\000\005\000\
\006\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\056\000\054\000\000\000\003\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\021\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\049\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\041\000\000\000\000\000\000\000\
\000\000\047\000\007\000\000\000\000\000\000\000\011\000\000\000\
\010\000\000\000\022\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\000\000\000\016\000\000\000\028\000\
\029\000\000\000\000\000\000\000\000\000\000\000\042\000\043\000\
\000\000\045\000\000\000\012\000\000\000\000\000\034\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\037\000\000\000\000\000\
\000\000\000\000\000\000\035\000\030\000\031\000\025\000\000\000\
\000\000\000\000\019\000\000\000\000\000\000\000\032\000\023\000\
\024\000\000\000\014\000\000\000\000\000\000\000\000\000\018\000\
\000\000\000\000\000\000\000\000\017\000\000\000\051\000\000\000\
\000\000\033\000\000\000\000\000\015\000"

let yydgoto = "\002\000\
\010\000\071\000\046\000\011\000\012\000\013\000\014\000\015\000\
\016\000\017\000\109\000\140\000\149\000\150\000\018\000\043\000\
\083\000\044\000\100\000\101\000"

let yysindex = "\012\000\
\044\255\000\000\229\254\248\254\038\255\002\255\007\255\011\255\
\015\255\000\000\066\255\065\000\070\255\066\255\000\000\000\000\
\000\000\058\255\043\255\067\255\065\255\087\255\255\254\098\255\
\078\255\135\000\000\000\000\000\000\000\036\255\000\000\094\255\
\243\254\094\255\121\255\096\255\100\255\094\255\000\000\101\255\
\104\255\105\255\000\000\036\255\109\255\102\255\106\255\094\255\
\108\255\127\255\110\255\112\255\116\255\115\255\089\255\089\255\
\000\000\094\255\122\255\145\255\117\255\000\255\113\255\126\255\
\140\255\128\255\089\255\118\255\000\000\131\255\124\255\129\255\
\125\255\000\000\000\000\123\255\135\255\154\255\000\000\137\255\
\000\000\018\255\000\000\000\000\132\255\130\255\133\255\150\255\
\089\255\152\255\143\255\000\000\155\255\000\000\018\255\000\000\
\000\000\018\255\238\254\099\255\249\254\140\255\000\000\000\000\
\134\255\000\000\136\255\000\000\138\255\139\255\000\000\250\254\
\141\255\089\255\018\255\018\255\018\255\018\255\089\255\089\255\
\144\255\140\255\140\255\054\255\147\255\000\000\146\255\148\255\
\091\255\075\255\157\255\000\000\000\000\000\000\000\000\156\255\
\158\255\149\255\000\000\159\255\151\255\089\255\000\000\000\000\
\000\000\153\255\000\000\163\255\160\255\161\255\162\255\000\000\
\164\255\174\255\151\255\180\255\000\000\165\255\000\000\166\255\
\143\255\000\000\168\255\170\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\072\255\192\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\045\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\169\255\000\000\000\000\000\000\000\000\069\255\000\000\173\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\172\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\049\255\103\255\000\000\177\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\177\255\177\255\181\255\000\000\000\000\097\255\000\000\
\063\255\085\255\083\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\178\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\182\255\000\000\000\000\
\000\000\000\000\178\255\000\000\000\000\000\000\000\000\000\000\
\172\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\200\255\230\255\184\000\105\000\000\000\000\000\000\000\
\000\000\000\000\040\000\000\000\047\000\000\000\000\000\159\000\
\171\255\000\000\167\255\203\255"

let yytablesize = 275
let yytable = "\073\000\
\048\000\072\000\072\000\035\000\078\000\111\000\047\000\049\000\
\112\000\113\000\085\000\053\000\001\000\072\000\019\000\115\000\
\121\000\114\000\116\000\117\000\118\000\061\000\048\000\119\000\
\120\000\129\000\130\000\131\000\132\000\079\000\126\000\074\000\
\106\000\020\000\036\000\072\000\136\000\137\000\095\000\021\000\
\096\000\097\000\040\000\022\000\003\000\004\000\005\000\006\000\
\023\000\007\000\041\000\042\000\024\000\098\000\008\000\068\000\
\025\000\128\000\069\000\099\000\072\000\009\000\138\000\139\000\
\027\000\133\000\134\000\004\000\005\000\006\000\003\000\007\000\
\031\000\055\000\055\000\055\000\008\000\055\000\046\000\040\000\
\040\000\046\000\055\000\009\000\038\000\151\000\030\000\038\000\
\072\000\055\000\040\000\038\000\038\000\040\000\040\000\040\000\
\115\000\040\000\040\000\038\000\117\000\118\000\032\000\040\000\
\036\000\040\000\033\000\036\000\036\000\039\000\038\000\036\000\
\036\000\039\000\039\000\026\000\117\000\118\000\029\000\036\000\
\115\000\039\000\034\000\116\000\117\000\118\000\068\000\043\000\
\043\000\069\000\070\000\026\000\026\000\037\000\039\000\045\000\
\050\000\051\000\059\000\055\000\056\000\052\000\054\000\058\000\
\062\000\063\000\064\000\060\000\065\000\066\000\067\000\075\000\
\076\000\077\000\080\000\081\000\082\000\084\000\087\000\086\000\
\088\000\090\000\091\000\089\000\092\000\093\000\094\000\103\000\
\102\000\105\000\107\000\108\000\110\000\135\000\104\000\122\000\
\153\000\123\000\124\000\141\000\125\000\142\000\127\000\118\000\
\143\000\144\000\158\000\145\000\147\000\146\000\160\000\053\000\
\148\000\154\000\152\000\155\000\028\000\027\000\156\000\165\000\
\163\000\159\000\057\000\000\000\161\000\157\000\027\000\162\000\
\164\000\044\000\020\000\052\000\013\000\000\000\000\000\050\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\048\000\048\000\048\000\000\000\048\000\000\000\
\000\000\000\000\000\000\048\000\000\000\000\000\000\000\000\000\
\000\000\000\000\048\000"

let yycheck = "\056\000\
\000\000\055\000\056\000\005\001\005\001\095\000\020\001\034\000\
\098\000\028\001\067\000\038\000\001\000\067\000\042\001\022\001\
\102\000\036\001\025\001\026\001\027\001\048\000\036\001\031\001\
\032\001\115\000\116\000\117\000\118\000\030\001\037\001\058\000\
\089\000\042\001\036\001\089\000\122\000\123\000\021\001\002\001\
\023\001\024\001\007\001\042\001\001\001\002\001\003\001\004\001\
\042\001\006\001\015\001\016\001\042\001\036\001\011\001\038\001\
\042\001\114\000\041\001\042\001\114\000\018\001\009\001\010\001\
\000\000\119\000\120\000\002\001\003\001\004\001\001\001\006\001\
\030\001\002\001\003\001\004\001\011\001\006\001\034\001\031\001\
\032\001\037\001\011\001\018\001\022\001\142\000\029\001\025\001\
\142\000\018\001\022\001\029\001\030\001\025\001\026\001\027\001\
\022\001\029\001\030\001\037\001\026\001\027\001\036\001\035\001\
\022\001\037\001\042\001\025\001\026\001\025\001\033\001\029\001\
\030\001\029\001\030\001\011\000\026\001\027\001\014\000\037\001\
\022\001\037\001\036\001\025\001\026\001\027\001\038\001\031\001\
\032\001\041\001\042\001\029\001\030\001\036\001\000\000\042\001\
\016\001\042\001\037\001\036\001\036\001\042\001\042\001\035\001\
\037\001\019\001\037\001\042\001\037\001\034\001\036\001\030\001\
\008\001\037\001\042\001\030\001\017\001\030\001\028\001\042\001\
\037\001\037\001\040\001\035\001\030\001\012\001\030\001\038\001\
\037\001\020\001\019\001\029\001\018\001\030\001\042\001\042\001\
\014\001\042\001\041\001\033\001\042\001\036\001\042\001\027\001\
\037\001\030\001\013\001\030\001\030\001\041\001\011\001\000\000\
\042\001\034\001\042\001\035\001\013\000\029\001\037\001\030\001\
\161\000\155\000\044\000\255\255\040\001\042\001\030\001\042\001\
\041\001\037\001\030\001\034\001\041\001\255\255\255\255\034\001\
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
# 69 "Surface_Parser.mly"
                                        (AST(_1, _2))
# 333 "../Surface_Parser.ml"
               : flowlog_ast))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 70 "Surface_Parser.mly"
                            (AST([], _1))
# 340 "../Surface_Parser.ml"
               : flowlog_ast))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 72 "Surface_Parser.mly"
                                (_2)
# 347 "../Surface_Parser.ml"
               : 'import))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'reactive_stmt) in
    Obj.repr(
# 75 "Surface_Parser.mly"
                            ([SReactive(_1)])
# 354 "../Surface_Parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decl_stmt) in
    Obj.repr(
# 76 "Surface_Parser.mly"
                        ([SDecl(_1)])
# 361 "../Surface_Parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rule_stmt) in
    Obj.repr(
# 77 "Surface_Parser.mly"
                        (List.map (fun r -> SRule(r)) _1)
# 368 "../Surface_Parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string list) in
    Obj.repr(
# 80 "Surface_Parser.mly"
                                                           (DeclTable(_2, _4))
# 376 "../Surface_Parser.ml"
               : 'decl_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string list) in
    Obj.repr(
# 81 "Surface_Parser.mly"
                                                                  (DeclRemoteTable(_3, _5))
# 384 "../Surface_Parser.ml"
               : 'decl_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string list) in
    Obj.repr(
# 82 "Surface_Parser.mly"
                                                           (DeclEvent(_2, _4))
# 392 "../Surface_Parser.ml"
               : 'decl_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 83 "Surface_Parser.mly"
                                                         (DeclInc(_2, _4))
# 400 "../Surface_Parser.ml"
               : 'decl_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string list) in
    Obj.repr(
# 84 "Surface_Parser.mly"
                                                              (DeclOut(_2, _4))
# 408 "../Surface_Parser.ml"
               : 'decl_stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "Surface_Parser.mly"
                    (())
# 414 "../Surface_Parser.ml"
               : 'optional_colon))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "Surface_Parser.mly"
                           (())
# 420 "../Surface_Parser.ml"
               : 'optional_colon))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 3 : 'optional_colon) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'refresh_clause) in
    Obj.repr(
# 95 "Surface_Parser.mly"
              (ReactRemote(_3, _5, _7, _9, _10))
# 432 "../Surface_Parser.ml"
               : 'reactive_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 15 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 13 : string list) in
    let _9 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _11 = (Parsing.peek_val __caml_parser_env 6 : 'possempty_assign_list) in
    let _14 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _15 = (Parsing.peek_val __caml_parser_env 2 : 'optional_colon) in
    let _16 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 99 "Surface_Parser.mly"
              (ReactOut(_2, _4, _9, _11, OutSend(_14, _16)))
# 445 "../Surface_Parser.ml"
               : 'reactive_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 101 "Surface_Parser.mly"
              (ReactInc(_2, _6))
# 453 "../Surface_Parser.ml"
               : 'reactive_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 103 "Surface_Parser.mly"
                                 ({afield=_1; atupvar=_3})
# 461 "../Surface_Parser.ml"
               : 'assign))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 106 "Surface_Parser.mly"
                                  (RefreshTimeout(int_of_string(_2), _3))
# 469 "../Surface_Parser.ml"
               : 'refresh_clause))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "Surface_Parser.mly"
                   (RefreshPure)
# 475 "../Surface_Parser.ml"
               : 'refresh_clause))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "Surface_Parser.mly"
              (RefreshEvery)
# 481 "../Surface_Parser.ml"
               : 'refresh_clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'on_clause) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'action_clause_list) in
    Obj.repr(
# 111 "Surface_Parser.mly"
    (map (fun act -> let triggerrel, triggervar, optwhere = _1 in 
           Rule(triggerrel, triggervar, (add_conjunct_to_action act optwhere))) _3)
# 490 "../Surface_Parser.ml"
               : 'rule_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'optional_fmla) in
    Obj.repr(
# 114 "Surface_Parser.mly"
                                                      ((_2,_4,_6))
# 499 "../Surface_Parser.ml"
               : 'on_clause))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : term list) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'optional_fmla) in
    Obj.repr(
# 117 "Surface_Parser.mly"
                                                                               (ADelete(_6, _3, _7))
# 508 "../Surface_Parser.ml"
               : 'action_clause))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : term list) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'optional_fmla) in
    Obj.repr(
# 118 "Surface_Parser.mly"
                                                                               (AInsert(_6, _3, _7))
# 517 "../Surface_Parser.ml"
               : 'action_clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : term list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'optional_fmla) in
    Obj.repr(
# 119 "Surface_Parser.mly"
                                                                      (ADo(_2, _4, _6))
# 526 "../Surface_Parser.ml"
               : 'action_clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 122 "Surface_Parser.mly"
                            (_2)
# 533 "../Surface_Parser.ml"
               : 'optional_fmla))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "Surface_Parser.mly"
              (FTrue)
# 539 "../Surface_Parser.ml"
               : 'optional_fmla))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "Surface_Parser.mly"
                   (FTrue)
# 545 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "Surface_Parser.mly"
                    (FFalse)
# 551 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 128 "Surface_Parser.mly"
                               (FEquals(_1, _3))
# 559 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 129 "Surface_Parser.mly"
                                  (FNot(FEquals(_1, _3)))
# 567 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : term list) in
    Obj.repr(
# 130 "Surface_Parser.mly"
                                           (FAtom("", _1, _3))
# 575 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : term list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 131 "Surface_Parser.mly"
                                                               (FAtom(_1, _3, _5))
# 585 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 132 "Surface_Parser.mly"
                          (FNot(_2))
# 592 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 133 "Surface_Parser.mly"
                                  (FAnd(_1, _3))
# 600 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 134 "Surface_Parser.mly"
                                 (FOr(_1, _3))
# 608 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 135 "Surface_Parser.mly"
                                    (_2)
# 615 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 136 "Surface_Parser.mly"
                                      (FOr(FNot(_1), _3))
# 623 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 137 "Surface_Parser.mly"
                                  (FOr(FAnd(_1, _3), FAnd(FNot(_1), FNot(_3))))
# 631 "../Surface_Parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 139 "Surface_Parser.mly"
                   (TVar(_1))
# 638 "../Surface_Parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 140 "Surface_Parser.mly"
                     (TConst(_1))
# 645 "../Surface_Parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 141 "Surface_Parser.mly"
                                           (TConst(_2))
# 652 "../Surface_Parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 142 "Surface_Parser.mly"
                               (TField(_1, _3))
# 660 "../Surface_Parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 145 "Surface_Parser.mly"
                   ([_1])
# 667 "../Surface_Parser.ml"
               : term list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : term list) in
    Obj.repr(
# 146 "Surface_Parser.mly"
                                   (_1 :: _3)
# 675 "../Surface_Parser.ml"
               : term list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 149 "Surface_Parser.mly"
                   ([_1])
# 682 "../Surface_Parser.ml"
               : string list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string list) in
    Obj.repr(
# 150 "Surface_Parser.mly"
                                   (_1 :: _3)
# 690 "../Surface_Parser.ml"
               : string list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'action_clause) in
    Obj.repr(
# 153 "Surface_Parser.mly"
                            ([_1])
# 697 "../Surface_Parser.ml"
               : 'action_clause_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'action_clause) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'action_clause_list) in
    Obj.repr(
# 154 "Surface_Parser.mly"
                                               (_1 :: _2)
# 705 "../Surface_Parser.ml"
               : 'action_clause_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assign) in
    Obj.repr(
# 157 "Surface_Parser.mly"
                     ([_1])
# 712 "../Surface_Parser.ml"
               : 'possempty_assign_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assign) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'possempty_assign_list) in
    Obj.repr(
# 158 "Surface_Parser.mly"
                                                 (_1 :: _3)
# 720 "../Surface_Parser.ml"
               : 'possempty_assign_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 159 "Surface_Parser.mly"
              ([])
# 726 "../Surface_Parser.ml"
               : 'possempty_assign_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 162 "Surface_Parser.mly"
                   (_1)
# 733 "../Surface_Parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 163 "Surface_Parser.mly"
                             (_1 @ _2)
# 741 "../Surface_Parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'import) in
    Obj.repr(
# 166 "Surface_Parser.mly"
                     ([_1])
# 748 "../Surface_Parser.ml"
               : 'import_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'import) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'import_list) in
    Obj.repr(
# 167 "Surface_Parser.mly"
                                 (_1 :: _2)
# 756 "../Surface_Parser.ml"
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
