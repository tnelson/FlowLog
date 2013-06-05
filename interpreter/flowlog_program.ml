module Program = struct

(* First string is name, second is list of arguments, third is body *)
type clause = Clause of string * string list * string list;;

(* First string is relation's name, the clauses are definitional clauses, the two options are plus_ and minus_ relations or none respectively  *)
type relation = Relation of string * clause list * relation option * relation option;;


end