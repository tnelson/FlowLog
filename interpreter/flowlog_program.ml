module Program = struct

(* First string is name, second is list of arguments, third is body *)
type clause = Clause of string * string list * string list;;
type relation = Relation of string * clause list;;


end