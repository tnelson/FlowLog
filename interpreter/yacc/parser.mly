%{
  open Flowlog_Types.Syntax;;
  open Type_Helpers.Parsing;;
%}
  %token EOF
  %token IMPORT
  %token <string> NAME
  %token PERIOD
  %token BLACKBOX
  %token AMPERSAND
  %token <string> NUMBER
  %token MODULE
  %token COLON
  %token TYPE
  %token EQUALS
  %token LCURLY
  %token RCURLY
  %token COMMA
  %token LPAREN
  %token RPAREN
  %token COLON_HYPHEN
  %token NOT
  %token <bool> BOOLEAN
  %start main

  %type <program> main
  %type <string list> import_list
  %type <string> import
  %type <blackbox list> blackbox_list
  %type <blackbox> blackbox
  %type <string> module_decl
  %type <notif_type list> type_decl_list
  %type <notif_type> type_decl
  %type <string list> name_list
  %type <clause list> clause_list
  %type <clause> clause
  %type <argument list> notif_term_arg_list
  %type <argument> notif_arg
  %type <literal list> literal_list
  %type <literal> literal
  %type <atom> atom
  %type <term> term
  %type <term list> term_list
  %%
  main:
      import_list blackbox_list module_decl type_decl_list clause_list EOF { make_Program $3 $1 $2 $4 $5 }
  ;
  import_list:
      import { [$1] }
    | import import_list { $1 :: $2 }
  ;
  import:
      IMPORT NAME PERIOD { $2 }
  ;
  blackbox_list:
      blackbox { [$1] }
    | blackbox blackbox_list { $1 :: $2 }
  ;
  blackbox:
      BLACKBOX NAME AMPERSAND NUMBER NUMBER PERIOD { make_External_BB $2 $4 (int_of_string $5) }
    | BLACKBOX NAME PERIOD { make_Internal_BB $2 }
  ;
  module_decl:
      MODULE NAME COLON { $2 }
  ;
  type_decl_list:
      type_decl { [$1] }
    | type_decl type_decl_list { $1 :: $2 }
  ;
  type_decl:
    TYPE NAME EQUALS LCURLY name_list RCURLY PERIOD { make_Type $2 $5 }
  ;
  name_list:
      NAME { [$1] }
    | NAME COMMA name_list { $1 :: $3 }
  ;
  clause_list:
      clause { [$1] }
    | clause clause_list { $1 :: $2 }
  ;
  clause:
      NAME LPAREN notif_term_arg_list RPAREN COLON_HYPHEN literal_list PERIOD { make_Plus_Minus_Clause $1 $3 $6 }
    | NAME LPAREN name_list RPAREN COLON_HYPHEN literal_list PERIOD { make_HelperClause $1 (List.map (fun str -> make_Arg_term(make_Variable(str))) $3) $6 }
    | NAME LPAREN notif_arg notif_arg RPAREN COLON_HYPHEN literal_list PERIOD { make_NotifClause $1 [$3; $4] $7 }
  ;
  notif_term_arg_list:
      notif_arg { [$1] }
    | notif_arg COMMA name_list { $1 :: List.map (fun str -> make_Arg_term (make_Variable str)) $3 }
  ;
  notif_arg:
      NAME COLON NAME { make_Arg_notif (make_Notif_var $3 $1) }
  ;
  literal_list:
      literal { [$1] }
    | literal COMMA literal_list { $1 :: $3 }
  ;
  literal:
      atom { Pos($1) }
    | NOT atom { Neg($2) }
  ;
  atom:
      term EQUALS term { Equals($1, $3) }
    | NAME LPAREN term_list RPAREN { make_Apply $1 $3 }
    | NAME PERIOD NAME LPAREN term_list RPAREN { make_Query $1 $3 $5 }
    | BOOLEAN { Bool($1) }
  ;
  term:
      NAME { make_Constant_Variable $1 }
    | NAME PERIOD NAME { make_Field_ref $1 $3 }
  ;
  term_list:
      term { [$1] }
    | term COMMA term_list { $1 :: $3 }
  ;