%{
  open Flowlog_Types
  open ExtList.List
%}


  %token EOF

  %token INCLUDE
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
  %token XOR
  %token TRUE
  %token FALSE
  %token IFF
  %token OR
  %token AND
  %token IN
  %token SLASH
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
  %token UNTIL
  %token THEN
  %token FORWARD
  %token STASH
  %token <string> QUOTED_IDENTIFIER
  %token <bool> BOOLEAN
  %token <string> NUMBER
  %token <string> NAME

  %start main

  %type <term list> term_list
  %type <string list> name_list

  %left IFF
  %left IMPLIES
  %left OR
  %nonassoc XOR
  %left AND
  %nonassoc NOT
  %right EQUALS NOTEQUALS

  %type <flowlog_ast> main

  %%

  main:
            | include_list EOF {{includes=$1; statements=[]}}
            | include_list stmt_list EOF {{includes=$1; statements=$2}}
            | stmt_list EOF { {includes=[]; statements=$1} };;

  include_:
            | INCLUDE QUOTED_IDENTIFIER SEMICOLON {$2};

  stmt:
            | reactive_stmt {[SReactive($1)]}
            | decl_stmt {[SDecl($1)]}
            | rule_stmt {map (fun r -> SRule(r)) $1};

  decl_stmt:
            | TABLE NAME LPAREN name_list RPAREN SEMICOLON {DeclTable($2, $4)}
            | REMOTE TABLE NAME LPAREN name_list RPAREN SEMICOLON {DeclRemoteTable($3, $5)}
            | EVENT NAME LCURLY field_decl_list RCURLY SEMICOLON {DeclEvent($2, $4)}
            | INCOMING NAME LPAREN NAME RPAREN SEMICOLON {DeclInc($2, $4)}
            | OUTGOING NAME LPAREN NAME RPAREN SEMICOLON {DeclOut($2, FixedEvent($4))};

  optional_colon:
            | COLON {()} | {()};

  reactive_stmt:
            | REMOTE TABLE NAME LPAREN name_list RPAREN FROM NAME AT
              NUMBER optional_colon NUMBER refresh_clause SEMICOLON
              {ReactRemote($3, $5, $8, $10, $12, $13)}
            | OUTGOING NAME LPAREN NAME RPAREN THEN
              SEND TO NUMBER optional_colon NUMBER SEMICOLON
              {ReactOut($2, FixedEvent($4), OutSend($4, $9, $11))}
            | INCOMING NAME THEN INSERT INTO NAME SEMICOLON
              {ReactInc($2, $6)};

  refresh_clause:
            | TIMEOUT NUMBER NAME {RefreshTimeout(int_of_string($2), $3)}
            | PURE {RefreshPure}
            | {RefreshEvery};

  rule_stmt: on_clause COLON action_clause_list
    {map (fun act -> let triggerrel, triggervar, optwhere = $1 in
           {onrel=triggerrel; onvar=triggervar; action=(add_conjunct_to_action act optwhere)}) $3};

  on_clause: ON NAME LPAREN NAME RPAREN optional_where {($2,$4,$6)};

  action_clause:
            | DELETE LPAREN term_list RPAREN FROM NAME optional_where SEMICOLON {ADelete($6, $3, $7)}
            | INSERT LPAREN term_list RPAREN INTO NAME optional_where SEMICOLON {AInsert($6, $3, $7)}
            | DO FORWARD LPAREN term RPAREN optional_where optional_timeout SEMICOLON {AForward($4, $6, $7)}
            | STASH LPAREN term RPAREN optional_where until_clause optional_then SEMICOLON {AStash($3, $5, $6, $7)};
            | DO NAME LPAREN term_list RPAREN optional_where SEMICOLON {ADo($2, $4, $6)};

  until_clause: UNTIL formula {$2};

  optional_where:
            | WHERE formula {$2}
            | {FTrue};
  optional_timeout:
            | TIMEOUT NUMBER {Some(int_of_string $2)}
            | {None};
  optional_then:
            | THEN action_clause_list {$2}
            | {[]};

  formula:
            | TRUE {FTrue}
            | FALSE {FFalse}
            | term EQUALS term {FEquals($1, $3)}
            | term NOTEQUALS term {FNot(FEquals($1, $3))}
            | NAME LPAREN term_list RPAREN {FAtom("", $1, $3)}
            | NAME PERIOD NAME LPAREN term_list RPAREN ON NAME {FAtom($1, $3, $5)}
            | NOT formula {FNot($2)}
            | formula AND formula {FAnd($1, $3)}
            | formula OR formula {FOr($1, $3)}
            | LPAREN formula RPAREN {$2}
            | term IN term SLASH term { FIn($1, $3, $5) }
            // XOR, IFF, and IMPLIES supported as sugar in parser
            | formula IMPLIES formula {FOr(FNot($1), $3)}
            | formula XOR formula {FAnd(FOr($1, $3), FNot(FAnd($1, $3)))}
            | formula IFF formula {FOr(FAnd($1, $3), FAnd(FNot($1), FNot($3)))};
  term:
            | NAME {TVar($1)}
            | NUMBER {TConst($1)}
             // (a) Don't include wrapper quotes in const, or XSB will happily create a Prolog list...
             // (b) Escape + wrap in single quotes when passing to XSB (in string_of_term)
            | QUOTED_IDENTIFIER {TConst(String.sub $1 1 (String.length $1 - 2))}
            | NAME PERIOD NAME {TField($1, $3)};

  term_list:
            | term {[$1]}
            | term COMMA term_list {$1 :: $3};

  name_list:
            | NAME {[$1]}
            | NAME COMMA name_list {$1 :: $3};

  field_decl_list:
            | NAME COLON NAME {[($1, $3)]}
            | NAME COLON NAME COMMA field_decl_list {($1, $3) :: $5};

  action_clause_list:
            | action_clause {[$1]}
            | action_clause action_clause_list {$1 :: $2};

  stmt_list:
            | stmt {$1}
            | stmt stmt_list {$1 @ $2};

  include_list:
            | include_ {[$1]}
            | include_ include_list {$1 :: $2};
