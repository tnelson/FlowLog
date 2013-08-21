%{
  (*open Types.Types;;
  open Type_Helpers;;*)


  type term = 
              | TConst of string 
              | TVar of string 
              | TField of string * string;;  

  type formula = 
              | FTrue 
              | FFalse 
              | FEquals of formula * formula 
              | FNot of formula 
              | FAtom of string * term list 
              | FAnd of formula * formula 
              | FOr of formula * formula;

  type action = 
              | ADelete of string * string list * formula 
              | AInsert of string * string list * formula 
              | ADo of string * string list * formula;

  type refresh = 
      | RefreshTimeout of int 
      | RefreshPure
      | RefreshEvery;

  type assignment = 
      | Assign string * string;

  type sreactive = 
      | ReactRemote of string * string list * spec_remote option;
      | ReactOut of string * string list * spec_out option;
      | ReactIn of string * string * spec_in option;

  type spec_remote = 
      | Remote of string * int * int * refresh;
  type spec_out = 
      | ReactSend of string * assignment list * int * int;
  type spec_in =
      | ReactInsert of string;

  type sdecl = 
      | DeclTable of string * string list    
      | DeclEvent of string * string list;
  type srule = 
      | Rule of string * string * action;
  type stmt = 
      | sreactive | sdecl | srule;

  type flowlog_ast = 
      | AST of string list * stmt list;

%}


  %token EOF

  %token IMPORT  
  %token TABLE
  %token REMOTE
  %token OUTGOING
  %token INCOMING  
  %token DO
  %token AT
  %token TIMEOUT  
  %token PURE
  %token ON
  %token SEND
  %token TO
  %token COLONEQUALS
  %token DELETE
  %token INSERT
  %token WHERE
  %token EVENT
  %token INTO
  %token FROM
  %token NOT
  %token IMPLIES
  %token TRUE
  %token FALSE
  %token IFF
  %token OR
  %token AND  
  %token PERIOD
  %token COLON
  %token SEMICOLON
  %token EQUALS
  %token NOTEQUALS
  %token LCURLY
  %token RCURLY
  %token COMMA
  %token LPAREN
  %token RPAREN
  %token DOUBLEQUOTE
  %token <bool> BOOLEAN
  %token <string> DOTTED_IP
  %token <string> NUMBER
  %token <string> NAME
  
  %start main

  %left AND 
  %left OR
  %left IMPLIES
  %left IFF
  %nonassoc NOT
  %right EQUALS NOTEQUALS

  %type <AST> main
  
  %%

  main: import_list stmt_list EOF {AST($1, $2)};

  import: IMPORT NAME SEMICOLON {$2};

  stmt: 
            | reactive_stmt {$1} 
            | decl_stmt {$1} 
            | rule_stmt {$1};

  decl_stmt: 
            | TABLE NAME LPAREN name_list RPAREN SEMICOLON {DeclTable($2, $4)} 
            | EVENT NAME LCURLY name_list RCURLY SEMICOLON {DeclEvent($2, $4)};

  reactive_stmt:              
            | REMOTE TABLE NAME LPAREN name_list RPAREN optional_remote_table SEMICOLON {ReactRemote($3, $5, $7)} 
            | OUTGOING NAME LPAREN name_list RPAREN optional_outgoing_then SEMICOLON {ReactOut($2, $4, 6)} 
            | INCOMING NAME LPAREN NAME RPAREN optional_incoming_then SEMICOLON {ReactInc($2, $4)};

  optional_remote_table: 
            | FROM NAME AT DOTTED_IP NUMBER refresh_clause {Some(Remote($2, $4, $5, $6))}
            | {None};

  optional_outgoing_then: 
            | SEND EVENT NAME LCURLY assign_list RCURLY TO DOTTED_IP NUMBER {Some(ReactSend($3, $5, $8, $9))}
            | {None};

  optional_incoming_then: 
            | INSERT INTO NAME {Some(ReactInsert($3))}
            | {None};

  assign: NAME COLONEQUALS NAME {Assign($1, $3)};

  refresh_clause:
            | TIMEOUT NUMBER {RefreshTimeout($2)} 
            | PURE {RefreshPure} 
            | {RefreshEvery};

  rule_stmt: on_clause COLON action_clause_list {List.map (fun act -> Rule(hd $1, hd (tl $1), act)) $3};

  on_clause: ON NAME LPAREN NAME RPAREN {[$2;$4]};

  action_clause: 
            | DELETE LPAREN name_list RPAREN FROM NAME optional_fmla SEMICOLON {ADelete($6, $3, $7)} 
            | INSERT LPAREN name_list RPAREN INTO NAME optional_fmla SEMICOLON {AInsert($6, $3, $7)} 
            | DO NAME LPAREN name_list RPAREN optional_fmla SEMICOLON {ADo($2, $4, $6)};  

  optional_fmla: 
            | WHERE formula {$2} 
            | {FTrue};

  formula: 
            | TRUE {FTrue}  
            | FALSE {FFalse} 
            | term EQUALS term {FEquals($1, $3)} 
            | term NOTEQUALS term {FNot(FEquals($1, $3))} 
            | NAME LPAREN term_list RPAREN {FAtom($1, $3)} 
            | NOT formula {FNot($2)} 
            | formula AND formula {FAnd($1, $3)} 
            | formula OR formula {FOr($1, $3)} 
            | LPAREN formula RPAREN {$2)} 
            | formula IMPLIES formula {FOr(FNot($1), $3)} 
            | formula IFF formula {FOr(FAnd($1, $3), FAnd(FNot($1), FNot($3)))};
  term: 
            | NAME {TVar($1)} 
            | NUMBER {TConst($1)} 
            | DOUBLEQUOTE NAME DOUBLEQUOTE {TConst($2)} 
            | NAME PERIOD NAME {TField($1, $3)]};

  term_list: 
            | term {[$1]} 
            | term COMMA term_list {$1 @ $3};

  name_list: 
            | NAME {[$1]} 
            | NAME COMMA name_list {$1 @ $3};

  action_clause_list: 
            | action_clause {[$1]}  
            | action_clause action_clause_list {$1 @ $2};

  assign_list: 
            | assign {[$1]} 
            | assign COMMA assign_list {$1 @ $3};

  stmt_list: 
            | stmt {[$1]} 
            | stmt stmt_list {$1 @ $2};

  import_list: 
            | import {[$1]} 
            | import import_list {$1 @ $2};
