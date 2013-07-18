%{
  open Types.Types;;
  (*open Type_Helpers.Parsing;;*)
  (*let parse_debug = false;;*)
  (* note: name changes have to happen here. Bring back Type_Helpers.Parsing.make_Program etc. *)
%}


  %token EOF

  %token IMPORT
  %token BLACKBOX
  %token MODULE
  %token TYPE
  %token PLUS
  %token MINUS
  %token STATE
  %token ACTION
  %token NOT
  %token <bool> BOOLEAN

  %token PERIOD
  %token AMPERSAND
  %token COLON_HYPHEN
  %token COLON
  %token SEMICOLON
  %token EQUALS
  %token LCURLY
  %token RCURLY
  %token COMMA
  %token LPAREN
  %token RPAREN
  %token DOUBLEQUOTE
  %token <string> DOTTED_IP
  %token <string> NUMBER
  %token <string> NAME
  %start main

  %type <program> main
  %type <string list * blackbox list> top
  %type <notif_type list * clause list> bottom
  %type <string> import
  %type <blackbox> blackbox
  %type <string> module_decl
  %type <notif_type> type_decl
  %type <string list> name_list
  %type <clause> clause
  %type <atom> atom
  %type <atom list> atom_list
  %type <term> term
  %type <term list> term_list
  %%
  main:
      top module_decl bottom EOF { match $1 with (imports, blackboxes) ->
        match $3 with (types, clauses) ->
        Program($2, imports, blackboxes, types, clauses) }
  ;
  top:
      import { ([$1], []) }
    | blackbox { ([], [$1]) }
    | import top { match $2 with (imports, blackboxes) -> ($1 :: imports, blackboxes) }
    | blackbox top { match $2 with (imports, blackboxes) -> (imports, $1 :: blackboxes) }
  ;
  bottom:
      type_decl { ([$1], []) }
    | clause { ([], [$1]) }
    | type_decl bottom { match $2 with (types, clauses) -> ($1 :: types, clauses) }
    | clause bottom { match $2 with (types, clauses) -> (types, $1 :: clauses) }
  ;
  import:
      IMPORT NAME SEMICOLON { $2 }
  ;
  blackbox:
      BLACKBOX NAME AMPERSAND DOTTED_IP NUMBER SEMICOLON { BlackBox(String.lowercase $2, External($4, (int_of_string $5))) }
    | BLACKBOX NAME SEMICOLON { BlackBox(String.lowercase $2, Internal) }
  ;
  module_decl:
      MODULE NAME COLON { String.lowercase $2 }
  ;
  type_decl:
    TYPE NAME EQUALS LCURLY name_list RCURLY SEMICOLON { Type(String.lowercase $2, $5) }
  ;
  name_list:
      NAME { [String.uppercase $1] }
    | NAME COMMA name_list { (String.uppercase $1) :: $3 }
  ;
  clause:
      PLUS NAME LPAREN term_list RPAREN COLON_HYPHEN atom_list SEMICOLON { Clause(Plus, String.lowercase $2, $4, $7) }
    | MINUS NAME LPAREN term_list RPAREN COLON_HYPHEN atom_list SEMICOLON { Clause(Minus, String.lowercase $2, $4, $7) }
    | STATE NAME LPAREN term_list RPAREN COLON_HYPHEN atom_list SEMICOLON { Clause(State, String.lowercase $2, $4, $7) }
    | STATE NAME LPAREN RPAREN COLON_HYPHEN atom_list SEMICOLON { Clause(State, String.lowercase $2, [], $6) }
    | ACTION NAME LPAREN term_list RPAREN COLON_HYPHEN atom_list SEMICOLON { Clause(Action, String.lowercase $2, $4 ,$7) }
  ;
  term_list:
      term { [$1] }
    | term COMMA term_list { $1 :: $3 }
  ;
  term:
      NAME { Variable(String.uppercase $1) }
    | NUMBER { Constant($1) }
    | DOUBLEQUOTE NAME DOUBLEQUOTE { Constant("constant_" ^ $2) (* WHAT IF THERE ARE SPACES? use String.map (fun c -> if c = ' ' then '_' else c) maybe? *)} 
    | NAME PERIOD NAME { Field_ref(String.uppercase $1, String.uppercase $3) }
    | NAME COLON NAME { Notiv_var(String.uppercase $1, String.uppercase $3) }
  ;
  atom:
      term EQUALS term { Equals(Pos, $1, $3) }
    | NOT term EQUALS term { Equals(Neg, $2, $2) }
    | NAME LPAREN term_list RPAREN { Apply(Pos, "", String.lowercase $1, $3) }
    | NOT NAME LPAREN term_list RPAREN { Apply(Neg, "", String.lowercase $2, $4) }
    | NAME LPAREN RPAREN { Apply(Pos, "", String.lowercase $1, []) }
    | NOT NAME LPAREN RPAREN { Apply(Neg, "", String.lowercase $2, []) }
    | NAME PERIOD NAME LPAREN term_list RPAREN { Apply(Pos, String.lowercase $1, String.lowercase $3, $5) }
    | NAME PERIOD NAME LPAREN RPAREN { Apply(Pos, String.lowercase $1, String.lowercase $3, []) }
    | NOT NAME PERIOD NAME LPAREN term_list RPAREN { Apply(Neg, String.lowercase $2, String.lowercase $4, $6) }
    | NOT NAME PERIOD NAME LPAREN RPAREN { Apply(Neg, String.lowercase $2, String.lowercase $4, []) }
    | BOOLEAN { Bool(Pos, $1) }
    | NOT BOOLEAN { Bool(Neg, $2) }
  ;
  atom_list:
      atom { [$1] }
    | atom COMMA atom_list { $1 :: $3 }
  ;
  
  