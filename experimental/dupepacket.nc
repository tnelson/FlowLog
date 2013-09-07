 
 (* compile_pred returned non boolean output: pass | pass *)

(*
 (if ((((((switch = 2 && dlDst = 00:00:00:00:00:01)
                   || (switch = 2 && !dlDst = 00:00:00:00:00:01) && !inPort = 1)
                  || (switch = 1 && !switch = 2) && !inPort = 1)
                 || (switch = 1 && !dlDst = 00:00:00:00:00:01) && !inPort = 1)
                || (switch = 3 && !switch = 2) && !inPort = 1)
               || (switch = 3 && !dlDst = 00:00:00:00:00:01) && !inPort = 1) 
      then fwd(1) else drop)
      +
      (if ((((((switch = 2 && !dlDst = 00:00:00:00:00:01) && !inPort = 2)
                 || (switch = 1 && !switch = 2) && !inPort = 2)
                || (switch = 1 && !dlDst = 00:00:00:00:00:01) && !inPort = 2)
               || (switch = 3 && !switch = 2) && !inPort = 2)
              || (switch = 3 && !dlDst = 00:00:00:00:00:01) && !inPort = 2)
       then fwd(2) else drop)
     +
     (if ((((switch = 2 && !dlDst = 00:00:00:00:00:01) && !inPort = 3)
              || (switch = 3 && !switch = 2) && !inPort = 3)
             || (switch = 3 && !dlDst = 00:00:00:00:00:01) && !inPort = 3) then fwd(3) else drop)
  
  *)  

(* dupe-packet Behavior occurs with filter/seq here: *)

(* 
(filter ((((((switch = 2 && dlDst = 00:00:00:00:00:01)
                   || (switch = 2 && !dlDst = 00:00:00:00:00:01) && !inPort = 1)
                  || (switch = 1 && !switch = 2) && !inPort = 1)
                 || (switch = 1 && !dlDst = 00:00:00:00:00:01) && !inPort = 1)
                || (switch = 3 && !switch = 2) && !inPort = 1)
               || (switch = 3 && !dlDst = 00:00:00:00:00:01) && !inPort = 1) 
      ; fwd(1)) 
      
      +
      (filter ((((((switch = 2 && !dlDst = 00:00:00:00:00:01) && !inPort = 2)
                 || (switch = 1 && !switch = 2) && !inPort = 2)
                || (switch = 1 && !dlDst = 00:00:00:00:00:01) && !inPort = 2)
               || (switch = 3 && !switch = 2) && !inPort = 2)
              || (switch = 3 && !dlDst = 00:00:00:00:00:01) && !inPort = 2)
       ; fwd(2) )
     +
     (filter ((((switch = 2 && !dlDst = 00:00:00:00:00:01) && !inPort = 3)
              || (switch = 3 && !switch = 2) && !inPort = 3)
             || (switch = 3 && !dlDst = 00:00:00:00:00:01) && !inPort = 3) ; 
             fwd(3) )

*)

  (* Differently-structured workaround. *)

  (if (switch = 2 && dlDst = 00:00:00:00:00:01) then fwd(1) 
  else if (switch = 2 && !dlDst = 00:00:00:00:00:01 && !inPort = 1) then fwd(1)
  else if (switch = 1 && !switch = 2 && !inPort = 1) then fwd(1)
  else if (switch = 1 && !dlDst = 00:00:00:00:00:01 && !inPort = 1) then fwd(1)
  else if (switch = 3 && !switch = 2 && !inPort = 1) then fwd(1)
  else if (switch = 3 && !dlDst = 00:00:00:00:00:01 && !inPort = 1) then fwd(1)
  else drop)      
      +
  (if (switch = 2 && !dlDst = 00:00:00:00:00:01 && !inPort = 2) then fwd(2)
  else if (switch = 1 && !switch = 2 && !inPort = 2) then fwd(2)
  else if (switch = 1 && !dlDst = 00:00:00:00:00:01 && !inPort = 2) then fwd(2)
  else if (switch = 3 && !switch = 2 && !inPort = 2) then fwd(2)
  else if (switch = 3 && !dlDst = 00:00:00:00:00:01 && !inPort = 2) then fwd(2)
  else drop)
     +
  (if (switch = 2 && !dlDst = 00:00:00:00:00:01 && !inPort = 3) then fwd(3)
   else if (switch = 3 && !switch = 2 && !inPort = 3) then fwd(3)
   else if (switch = 3 && !dlDst = 00:00:00:00:00:01 && !inPort = 3) then fwd(3) 
   else drop)