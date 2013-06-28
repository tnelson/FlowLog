%{
  open Flowlog
%}
  %token EOF
  %token <string> CLAUSE_NAME
  %token LPAREN RPAREN
  %token COLON_HYPHEN SEMICOLON COMMA
  %token <string> CONSTANT
  %token <string> VARIABLE
  %token NOT
  %token EQUALS
  %token <bool> BOOLEAN
  %start main
  %type <Flowlog.program> main
  %type <Flowlog.clause list> clause_list
  %type <Flowlog.clause> clause
  %type <Flowlog.term list> term_list
  %type <Flowlog.literal list> literal_list
  %type <Flowlog.atom> atom
  %%
  main:
      clause_list EOF { Flowlog.make_program $1 }
  ;
  clause_list:
      clause { [$1] }
    | clause clause_list { $1 :: $2 }
  ;
  clause:
      CLAUSE_NAME LPAREN term_list RPAREN COLON_HYPHEN literal_list SEMICOLON { Flowlog.Clause($1, $3, $6)}
  ;
  term_list:
      term { [$1] }
    | term COMMA term_list { $1 :: $3 }
  ;
  literal_list:
      literal { [$1] }
    | literal COMMA literal_list { $1 :: $3 }
  ;
  term:
      CONSTANT { Flowlog.Constant($1) }
    | VARIABLE { Flowlog.Variable($1) }
  ;
  literal:
      atom { Flowlog.Pos($1) }
    | NOT atom { Flowlog.Neg($2) }
  ;
  atom:
      term EQUALS term { Flowlog.Equals($1, $3) }
    | CLAUSE_NAME LPAREN term_list RPAREN { Flowlog.Apply($1, $3) }
    | BOOLEAN { Flowlog.Bool($1) }
  ;