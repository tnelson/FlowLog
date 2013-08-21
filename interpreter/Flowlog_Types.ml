  type term = 
              | TConst of string 
              | TVar of string 
              | TField of string * string;;  

  type formula = 
              | FTrue 
              | FFalse 
              | FEquals of term * term
              | FNot of formula 
              | FAtom of string * term list 
              | FAnd of formula * formula 
              | FOr of formula * formula;;

  type action = 
              | ADelete of string * string list * formula 
              | AInsert of string * string list * formula 
              | ADo of string * string list * formula;;

  type refresh = 
      (* number, units *)
      | RefreshTimeout of int * string
      | RefreshPure
      | RefreshEvery;;

  type assignment = 
      | Assign of string * string;;

  (* remote relation name -> remote IP -> remote port -> refresh options *)
  type spec_remote = 
      | Remote of string * string * string * refresh;;
  type spec_out = 
      | ReactSend of string * assignment list * string * string;;
  type spec_in =
      | ReactInsert of string;;

  type sreactive = 
      | ReactRemote of string * string list * spec_remote option
      | ReactOut of string * string list * spec_out option
      | ReactInc of string * string * spec_in option;;

  type sdecl = 
      | DeclTable of string * string list    
      | DeclEvent of string * string list;;
  type srule = 
      | Rule of string * string * action;;

  type stmt = 
      | SReactive of sreactive 
      | SDecl of sdecl 
      | SRule of srule;;

  type flowlog_ast = 
      | AST of string list * stmt list;;
