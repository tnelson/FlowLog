%{
  open Flowlog_Types;;
%}


  %token EOF

  %token IMPORT  
  %token TABLE
  %token REMOTE
  %token OUTGOING
  %token THEN
  %token INCOMING
  %token DO
  %token AT
  %token TIMEOUT  
  %token PURE
  %token ON
  %token SEND
  %token TO
  %token COLON_EQUALS
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

  %type <flowlog_ast> main
  
  %%

  main: 
            | import_list stmt_list EOF {AST($1, $2)}
            | stmt_list EOF {AST([], $1)};;

  import: IMPORT NAME SEMICOLON {$2};

  stmt: 
            | reactive_stmt {[SReactive($1)]} 
            | decl_stmt {[SDecl($1)]} 
            | rule_stmt {List.map (fun r -> SRule(r)) $1};

  decl_stmt: 
            | TABLE NAME LPAREN name_list RPAREN SEMICOLON {DeclTable($2, $4)} 
            | REMOTE TABLE NAME LPAREN name_list RPAREN SEMICOLON {DeclRemoteTable($3, $5)}
            | EVENT NAME LCURLY name_list RCURLY SEMICOLON {DeclEvent($2, $4)}
            | INCOMING NAME LPAREN NAME RPAREN SEMICOLON {DeclInc($2, $4)}
            | OUTGOING NAME LPAREN name_list RPAREN SEMICOLON {DeclOut($2, $4)};

// OUTGOING notify-police(mac, swid, t) THEN 
// SEND EVENT stolen-laptop-found {mac:=mac, swid:=swid, time:=t} TO 127.0.0.1 5050;

  reactive_stmt:              
            | REMOTE TABLE NAME FROM NAME AT DOTTED_IP NUMBER refresh_clause SEMICOLON 
              {ReactRemote($3, $5, $7, $8, $9)}             
            //| OUTGOING NAME LPAREN RPAREN PERIOD
            // THEN 
             // SEND EVENT NAME LCURLY possempty_assign_list RCURLY TO DOTTED_IP NUMBER SEMICOLON 
             // {ReactOut($2, $4, $9, $11, $14, $15)} 
             //{()}
            | INCOMING NAME THEN INSERT INTO NAME SEMICOLON 
              {ReactInc($2, $6)};
  
  assign: NAME COLON_EQUALS NAME {Assign($1, $3)};

  refresh_clause:
            | TIMEOUT NUMBER NAME {RefreshTimeout(int_of_string($2), $3)} 
            | PURE {RefreshPure} 
            | {RefreshEvery};

  rule_stmt: on_clause COLON action_clause_list 
    {List.map (fun act -> Rule(List.hd $1, List.hd (List.tl $1), act)) $3};

  on_clause: ON NAME LPAREN NAME RPAREN {[$2;$4]};

  action_clause: 
            | DELETE LPAREN term_list RPAREN FROM NAME optional_fmla SEMICOLON {ADelete($6, $3, $7)} 
            | INSERT LPAREN term_list RPAREN INTO NAME optional_fmla SEMICOLON {AInsert($6, $3, $7)} 
            | DO NAME LPAREN term_list RPAREN optional_fmla SEMICOLON {ADo($2, $4, $6)};  

  optional_fmla: 
            | WHERE formula {$2} 
            | {FTrue};

  formula: 
            | TRUE {FTrue}  
            | FALSE {FFalse} 
            | term EQUALS term {FEquals($1, $3)} 
            | term NOTEQUALS term {FNot(FEquals($1, $3))} 
            | NAME LPAREN term_list RPAREN {FAtom("", $1, $3)} 
            | NAME PERIOD NAME LPAREN term_list RPAREN {FAtom($1, $3, $5)} 
            | NOT formula {FNot($2)} 
            | formula AND formula {FAnd($1, $3)} 
            | formula OR formula {FOr($1, $3)} 
            | LPAREN formula RPAREN {$2} 
            | formula IMPLIES formula {FOr(FNot($1), $3)} 
            | formula IFF formula {FOr(FAnd($1, $3), FAnd(FNot($1), FNot($3)))};
  term: 
            | NAME {TVar($1)} 
            | NUMBER {TConst($1)} 
            | DOUBLEQUOTE NAME DOUBLEQUOTE {TConst($2)} 
            | NAME PERIOD NAME {TField($1, $3)};

  term_list: 
            | term {[$1]} 
            | term COMMA term_list {$1 :: $3};

  name_list: 
            | NAME {[$1]} 
            | NAME COMMA name_list {$1 :: $3};

  action_clause_list: 
            | action_clause {[$1]}  
            | action_clause action_clause_list {$1 :: $2};

  possempty_assign_list:          
            | assign {[$1]} 
            | assign COMMA possempty_assign_list {$1 :: $3}
            | {[]};

  stmt_list: 
            | stmt {$1} 
            | stmt stmt_list {$1 @ $2};

  import_list:             
            | import {[$1]} 
            | import import_list {$1 :: $2};
