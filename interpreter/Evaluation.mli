open Flowlog_Types;;

module Evaluation : sig
	val respond_to_notifications : Types.notif_val -> Types.program -> unit;;
end