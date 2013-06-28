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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Flowlog
# 20 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* COLON_HYPHEN *);
  261 (* SEMICOLON *);
  262 (* COMMA *);
  265 (* NOT *);
  266 (* EQUALS *);
    0|]

let yytransl_block = [|
  257 (* CLAUSE_NAME *);
  263 (* CONSTANT *);
  264 (* VARIABLE *);
  267 (* BOOLEAN *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\004\000\004\000\005\000\005\000\
\007\000\007\000\008\000\008\000\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\007\000\001\000\003\000\001\000\003\000\
\001\000\001\000\001\000\002\000\003\000\004\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\016\000\000\000\000\000\000\000\001\000\
\003\000\009\000\010\000\000\000\000\000\000\000\000\000\000\000\
\006\000\000\000\000\000\015\000\000\000\011\000\000\000\000\000\
\000\000\012\000\004\000\000\000\000\000\000\000\013\000\008\000\
\014\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\012\000\021\000\022\000\013\000\024\000"

let yysindex = "\001\000\
\014\255\000\000\016\255\000\000\019\000\014\255\254\254\000\000\
\000\000\000\000\000\000\018\255\017\255\020\255\254\254\000\255\
\000\000\023\255\009\255\000\000\021\255\000\000\012\255\022\255\
\254\254\000\000\000\000\254\254\000\255\024\255\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\029\000\000\000\000\000\
\000\000\000\000\000\000\000\000\027\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\026\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\026\000\000\000\245\255\004\000\015\000\240\255\000\000"

let yytablesize = 34
let yytable = "\023\000\
\018\000\001\000\023\000\017\000\010\000\011\000\010\000\011\000\
\019\000\018\000\020\000\031\000\023\000\030\000\003\000\010\000\
\011\000\007\000\008\000\020\000\014\000\028\000\015\000\016\000\
\025\000\027\000\033\000\029\000\002\000\005\000\007\000\009\000\
\032\000\026\000"

let yycheck = "\016\000\
\001\001\001\000\019\000\015\000\007\001\008\001\007\001\008\001\
\009\001\001\001\011\001\028\000\029\000\025\000\001\001\007\001\
\008\001\002\001\000\000\011\001\003\001\010\001\006\001\004\001\
\002\001\005\001\003\001\006\001\000\000\003\001\005\001\006\000\
\029\000\019\000"

let yynames_const = "\
  EOF\000\
  LPAREN\000\
  RPAREN\000\
  COLON_HYPHEN\000\
  SEMICOLON\000\
  COMMA\000\
  NOT\000\
  EQUALS\000\
  "

let yynames_block = "\
  CLAUSE_NAME\000\
  CONSTANT\000\
  VARIABLE\000\
  BOOLEAN\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Flowlog.clause list) in
    Obj.repr(
# 22 "parser.mly"
                      ( Flowlog.make_program _1 )
# 114 "parser.ml"
               : Flowlog.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Flowlog.clause) in
    Obj.repr(
# 25 "parser.mly"
             ( [_1] )
# 121 "parser.ml"
               : Flowlog.clause list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Flowlog.clause) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Flowlog.clause list) in
    Obj.repr(
# 26 "parser.mly"
                         ( _1 :: _2 )
# 129 "parser.ml"
               : Flowlog.clause list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Flowlog.term list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Flowlog.literal list) in
    Obj.repr(
# 29 "parser.mly"
                                                                              ( Flowlog.Clause(_1, _3, _6))
# 138 "parser.ml"
               : Flowlog.clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 32 "parser.mly"
           ( [_1] )
# 145 "parser.ml"
               : Flowlog.term list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Flowlog.term list) in
    Obj.repr(
# 33 "parser.mly"
                           ( _1 :: _3 )
# 153 "parser.ml"
               : Flowlog.term list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 36 "parser.mly"
              ( [_1] )
# 160 "parser.ml"
               : Flowlog.literal list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'literal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Flowlog.literal list) in
    Obj.repr(
# 37 "parser.mly"
                                 ( _1 :: _3 )
# 168 "parser.ml"
               : Flowlog.literal list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 40 "parser.mly"
               ( Flowlog.Constant(_1) )
# 175 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 41 "parser.mly"
               ( Flowlog.Variable(_1) )
# 182 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Flowlog.atom) in
    Obj.repr(
# 44 "parser.mly"
           ( Flowlog.Pos(_1) )
# 189 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Flowlog.atom) in
    Obj.repr(
# 45 "parser.mly"
               ( Flowlog.Neg(_2) )
# 196 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 48 "parser.mly"
                       ( Flowlog.Equals(_1, _3) )
# 204 "parser.ml"
               : Flowlog.atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Flowlog.term list) in
    Obj.repr(
# 49 "parser.mly"
                                          ( Flowlog.Apply(_1, _3) )
# 212 "parser.ml"
               : Flowlog.atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 50 "parser.mly"
              ( Flowlog.Bool(_1) )
# 219 "parser.ml"
               : Flowlog.atom))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Flowlog.program)
