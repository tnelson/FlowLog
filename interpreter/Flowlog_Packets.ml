open Flowlog_Types
open Flowlog_Helpers
open Packet
open Packet.Ip
open ExtList.List
open NetCore_Types
open Printf 

(**********************************************)
(**********************************************)
(**********************************************)

(* EDIT PACKET FLAVOR INFO HERE, BETWEEN THE TRIPLE-COMMENT LINES *)

(* TO ADD A NEW PACKET FLAVOR, ADD TO:
  (1) packet_flavors list
  (2) get_field_default (if defaults are desired)
  (3) marshal_packet 
  (4) pkt_to_event *)


(* This is likely to change if we change engines.*)
let field_is_defined (ev: event) (fname: string): bool = 
  not (starts_with (get_field ev fname) "_");;

let get_field_default (ev: event) (fname: string): string =
  (* Assuming that the field is THERE... it may be set to a "dunno" by xsb *)
  let get_default () = (match ev.typeid, fname with        
        | _,"nwsrc" -> "0"
        | _,"nwdst" -> "0"
        | _,"nwproto" -> "0"
        | _,"nwfrag" -> "0"
        | _,"nwttl" -> "0"
        | _,"nwtos" -> "0"      
        | _,"nwchksum" -> "0"
        | _,"nwident" -> "0"     
        | _,"tpsrc" -> "0"     
        | _,"tpdst" -> "0"     
        | _,"tpseq" -> "0"     
        | _,"tpack" -> "0"     
        | _,"tpoffset" -> "0"     
        | _,"tpwindow" -> "0"     
        | _,"tpchksum" -> "0"     
        | _,"tpurgent" -> "0"     
        | _ -> failwith ("get_field_default. no default specified for: "^fname)) in
  try
    let evval = (get_field ev fname) in      
      if field_is_defined ev fname then evval
      else get_default()
  with 
    | Not_found -> get_default();;

(*************************************************************)
(* Generalized notion of a flavor of packet, like ARP or ICMP. 
   Avoids ugly multiple lists of "built-ins" where we can forget to add packet types, etc.
   label = string which is prepended to "_packet_in" and appended to "emit_" to arrive at relations.
   superflavor = immediate supertype. If None, means that this is a direct subtype of an ethernet packet.
   condition = a function that, given a variable name, constructs a formula that describes that it means 
               to be a member of this flavor. E.g., to be an ip packet, you must have your dltyp field = 0x800.
   fields = New fields added by this flavor. Must not be present in any superflavors.   
 *)


type packet_flavor = { label: string;  superflavor: string option; 
                       build_condition: (string -> formula); fields: string list};;

let make_arp (ev: event): Packet.nw =
  let arp_sha = Int64.of_string (get_field ev "arp_sha") in 
  let arp_spa = Int32.of_string (get_field ev "arp_spa") in   
  let arp_tpa = Int32.of_string (get_field ev "arp_tpa") in     
  let arp_op = int_of_string (get_field ev "arp_op") in       
    match arp_op with
    | 1 -> (* query *)
      Arp(Arp.Query(arp_sha, arp_spa, arp_tpa))
    | 2 -> (* reply *)
      let arp_tha = Int64.of_string (get_field ev "arp_tha") in   
        Arp(Arp.Reply(arp_sha, arp_spa, arp_tha, arp_tpa))
    | _ -> failwith "bad arp op";;

let make_ip (ev: event) (tp: Ip.tp): Packet.nw =  
  let nwSrc = Int32.of_string (get_field_default ev "nwsrc") in 
  let nwDst = Int32.of_string (get_field_default ev "nwdst") in   
  let nwFrag = int_of_string (get_field_default ev "nwfrag") in   
  let nwTtl = int_of_string (get_field_default ev "nwttl") in   
  let nwTos = int_of_string (get_field_default ev "nwtos") in   
  let nwChksum = int_of_string (get_field_default ev "nwchksum") in   
  let nwIdent = int_of_string (get_field_default ev "nwident") in     
    Ip({src=nwSrc; dst=nwDst; 
     flags={Packet.Ip.Flags.df=false;Packet.Ip.Flags.mf=false}; frag=nwFrag;
     tos=nwTos; ident=nwIdent; ttl=nwTtl; chksum=nwChksum;
     tp=tp});;

let make_tcp (ev: event): Packet.Ip.tp =  
  let tpSrc = int_of_string (get_field_default ev "tpsrc") in 
  let tpDst = int_of_string (get_field_default ev "tpdst") in 
  let tpSeq = Int32.of_string (get_field_default ev "tpseq") in   
  let tpAck = Int32.of_string (get_field_default ev "tpseq") in   
  let tpOffset = int_of_string (get_field_default ev "tpoffset") in   
  (*let tpFlags = int_of_string (get_field_default ev "tpflags") in   *)
  let tpWindow = int_of_string (get_field_default ev "tpwindow") in   
  let tpChksum = int_of_string (get_field_default ev "tpchksum") in     
  let tpUrgent = int_of_string (get_field_default ev "tpurgent") in       
  let flags =  { Tcp.Flags.ns = false; cwr = false; ece = false; urg = false; ack = false;
                 psh = false; rst = false; syn= false; fin = false} in      
    Tcp({Tcp.src=tpSrc; dst=tpDst; 
     flags=flags; seq=tpSeq; ack=tpAck;offset=tpOffset;
     window=tpWindow;chksum=tpChksum;urgent=tpUrgent;
     (* empty payload *)
     payload=Cstruct.create 0});;

(* Avoid gyrations with variant types by having these separate from the flavor declarations.
  Originally wanted each flavor to carry its own marshal function, but due to different Packet 
  module types + heterogenous lists, it would have been involved.
  Probably a better way to do this. Kludging for now. *)

let marshal_packet (ev: event): Packet.bytes =  
  let prepare_ethernet (nw: Packet.nw): Packet.bytes = 
    let dlSrc = Int64.of_string (get_field_default ev "dlsrc") in 
    let dlDst = Int64.of_string (get_field_default ev "dldst") in     
      Packet.marshal {Packet.dlSrc = dlSrc; Packet.dlDst = dlDst;
                      Packet.dlVlan = None; Packet.dlVlanPcp = 0;             
                      nw = nw} in 
  match ev.typeid with
    | "packet" -> 
      let dlTyp = int_of_string (get_field_default ev "dltyp") in   
        prepare_ethernet (Packet.Unparsable(dlTyp, Cstruct.create(0)))
    | "ip_packet" ->
      let nwProto = int_of_string (get_field_default ev "nwproto") in   
        prepare_ethernet (make_ip ev (Packet.Ip.Unparsable(nwProto, Cstruct.create(0))))
    | "arp_packet" -> prepare_ethernet (make_arp ev)
    | "icmp_packet" -> failwith "icmp unsupported"
    | "igmp_packet" -> failwith "igmp unsupported"
    | "udp_packet" ->  failwith "udp unsupported"
    | "tcp_packet" -> prepare_ethernet (make_ip ev (make_tcp ev))
    | _ -> failwith ("marshal_packet: unknown type: "^ev.typeid);;

(* Fields in a base packet *)
let packet_fields = ["locsw";"locpt";"dlsrc";"dldst";"dltyp"];;

let packet_flavors = [
   (* base type *)
   {label=""; superflavor=None;
    build_condition=(fun vname -> FTrue); 
    fields=packet_fields};

   {label="arp"; superflavor=Some "";  
    build_condition=(fun vname -> FEquals(TField(vname, "dltyp"), TConst("0x0806"))); 
    fields=["arp_op";"arp_spa";"arp_sha";"arp_tpa";"arp_tha"]};
   
   {label="ip"; superflavor=Some "";  
    build_condition=(fun vname -> FEquals(TField(vname, "dltyp"), TConst("0x0800"))); 
    fields=["nwsrc"; "nwdst"; "nwproto"]};  (* missing: frag, tos, chksum, ident, ...*) 
  
  (* {label="lldp"; superflavor=None;  
    build_condition=(fun vname -> FEquals(TField(vname, "dltyp"), TConst("0x88CC"))); 
    fields=["omgwtfbbq"]}; *)
   
   {label="tcp"; superflavor=Some "ip";  
    build_condition=(fun vname -> FAnd(FEquals(TField(vname, "dltyp"), TConst("0x0800")), 
                                 FEquals(TField(vname, "nwproto"), TConst("0x6")))); 
    fields=["tpsrc"; "tpdst"]}; (* expect we'll want flags eventually *)  
   
   {label="udp"; superflavor=Some "ip";  
    build_condition=(fun vname -> FAnd(FEquals(TField(vname, "dltyp"), TConst("0x0800")), 
                                 FEquals(TField(vname, "nwproto"), TConst("0x11")))); 
    fields=["tpsrc"; "tpdst"]};

  (* {label="igmp"; superflavor=Some "ip";  
    build_condition=(fun vname -> FAnd(FEquals(TField(vname, "dltyp"), TConst("0x0800")), 
                                 FEquals(TField(vname, "nwproto"), TConst("0x2")))); 
    fields=["omgwtfbbq"]};*)

   {label="icmp"; superflavor=Some "ip";  
    build_condition=(fun vname -> FAnd(FEquals(TField(vname, "dltyp"), TConst("0x0800")), 
                                 FEquals(TField(vname, "nwproto"), TConst("0x1")))); 
    fields=["icmp_type"; "icmp_code"]}; (* checksum will need calculation in runtime? *)    
  ];;

(**********************************************)

let get_arp (pkt: Packet.packet): (string*string) list =
  match pkt.nw with
    | Arp(Arp.Query(arp_sha, arp_spa, arp_tpa)) ->        
      [("arp_sha", Int64.to_string arp_sha); ("arp_spa", Int32.to_string arp_spa); 
       ("arp_tpa", Int32.to_string arp_tpa); ("arp_op", "1");
       (* arp_packets all must have the same fields *)
       ("arp_tha", "0")]
    | Arp(Arp.Reply(arp_sha, arp_spa, arp_tha, arp_tpa)) -> 
      [("arp_sha", Int64.to_string arp_sha); ("arp_spa", Int32.to_string arp_spa); 
       ("arp_tpa", Int32.to_string arp_tpa); ("arp_op", "2");
       ("arp_tha", Int64.to_string arp_tha)]
    | Ip (_) -> []
    | _ -> [];;
  
let get_ip (pkt: Packet.packet): (string*string) list =    
  match pkt.nw with 
   | Ip(_) -> 
    [("nwsrc", Int32.to_string (Packet.nwSrc pkt));
     ("nwdst", Int32.to_string (Packet.nwDst pkt));
     ("nwproto", string_of_int (Packet.nwProto pkt))]
     | _ -> [];;    


let pkt_to_event (sw : switchId) (pt: port) (pkt : Packet.packet) : event =          
   let values = [
    ("locsw", Int64.to_string sw);
    ("locpt", NetCore_Pretty.string_of_port pt);
    ("dlsrc", Int64.to_string pkt.Packet.dlSrc);
    ("dldst", Int64.to_string pkt.Packet.dlDst);
    ("dltyp", string_of_int (Packet.dlTyp pkt))] 
    @ (get_arp pkt) 
    @ (get_ip pkt) in
    let typeid = (match (Packet.dlTyp pkt) with  
      | 0x0800 -> "ip_packet"
      | 0x0806 -> "arp_packet"
      | _ -> "packet") in   
    {typeid=typeid; values=construct_map values};;
    



(**********************************************)
(**********************************************)
(**********************************************)

(* Fields that OpenFlow permits modification of. *)
let legal_to_modify_packet_fields = ["locpt";"dlsrc";"dldst";"dltyp";"nwsrc";"nwdst"];;

let swpt_fields = ["sw";"pt"];;
let swdown_fields = ["sw"];;

(**********************************************)

let flavor_to_typename (flav: packet_flavor): string = if flav.label = "" then "packet" else flav.label^"_packet";;
let flavor_to_inrelname (flav: packet_flavor): string = if flav.label = "" then "packet_in" else flav.label^"_packet_in";;
let flavor_to_emitrelname (flav: packet_flavor): string = if flav.label = "" then "emit" else "emit_"^flav.label;;

(*************************************************************)

(* For efficiency *)
let map_from_typename_to_flavor: packet_flavor StringMap.t = 
  fold_left (fun acc flav -> StringMap.add (flavor_to_typename flav) flav acc) StringMap.empty packet_flavors;;
let map_from_label_to_flavor: packet_flavor StringMap.t = 
  fold_left (fun acc flav -> StringMap.add flav.label flav acc) StringMap.empty packet_flavors;;
let map_from_relname_to_flavor: packet_flavor StringMap.t = 
  fold_left (fun acc flav -> StringMap.add (flavor_to_emitrelname flav) flav
                                           (StringMap.add (flavor_to_inrelname flav) flav acc))
            StringMap.empty packet_flavors;;

(*************************************************************)

(* If adding a new packet type, make sure to include self and all supertypes here. *)
(* E.g. arp_packet always fires packet also. *)
let rec built_in_supertypes (typename: string): string list = 
  try
    let flav = StringMap.find typename map_from_typename_to_flavor in    
    match flav.superflavor with
      (* flavor has a label, not type name yet *)
      | Some superlabel -> 
        let superflav = StringMap.find superlabel map_from_label_to_flavor in          
          (*printf "supers: %s\n%!" (String.concat "," (typename::(built_in_supertypes (flavor_to_typename superflav))));*)
          typename::(built_in_supertypes (flavor_to_typename superflav))
      | None -> [typename] 
  with Not_found -> [typename];;

let flavor_to_fields (flav: packet_flavor): string list = 
  let typename = flavor_to_typename flav in
  let supertypes = built_in_supertypes typename in  
  let fieldslist = (fold_left (fun acc supertype -> acc @ (StringMap.find supertype map_from_typename_to_flavor).fields) [] supertypes) in
  let check_for_dupes = unique fieldslist in  
  if (length fieldslist) <> (length check_for_dupes) then
    failwith ("Packet flavor "^flav.label^" had duplicate fieldnames when parent flavor fields were added.")    
  else fieldslist;;

(*************************************************************)

(* We don't yet have access to vname until we have a concrete rule *)
(* Remember: field names must be lowercase *)
(* both INCOMING and OUTGOING relations can call this. *)
let built_in_where_for_variable (vart: term) (relname: string): formula = 
  let vname = (match vart with | TVar(x) -> x | _ -> failwith "built_in_where_for_vname") in
  try
    let flav = StringMap.find relname map_from_relname_to_flavor in
      (flav.build_condition vname)
  with Not_found -> FTrue ;;

(*************************************************************)

let create_id_assign (k: string): assignment = {afield=k; atupvar=k};;
let build_flavor_decls (flav: packet_flavor): sdecl list =   
  [DeclInc(flavor_to_inrelname flav, flavor_to_typename flav);
   DeclEvent(flavor_to_typename flav, flavor_to_fields flav); 
   DeclOut(flavor_to_emitrelname flav, [flavor_to_typename flav])];;
let build_flavor_reacts (flav: packet_flavor): sreactive list =   
  [ReactInc(flavor_to_typename flav, flavor_to_inrelname flav); 
   ReactOut(flavor_to_emitrelname flav, flavor_to_fields flav, flavor_to_typename flav, 
            map create_id_assign (flavor_to_fields flav), OutEmit(flavor_to_typename flav))];;

let built_in_decls = [DeclInc(switch_reg_relname, "switch_port"); 
                      DeclInc(switch_down_relname, "switch_down");
                      DeclInc(startup_relname, "startup");                      
                      DeclOut("forward", ["packet"]);                                          
                      DeclEvent("startup", []);
                      DeclEvent("switch_port", swpt_fields);                      
                      DeclEvent("switch_down", swdown_fields)]
                    @ flatten (map build_flavor_decls packet_flavors);;

let built_in_reacts = [ ReactInc("switch_port", switch_reg_relname); 
                        ReactInc("switch_down", switch_down_relname); 
                        ReactInc("startup", startup_relname);                                               
                        ReactOut("forward", packet_fields, "packet", map create_id_assign packet_fields, OutForward);                      
                      ] @ flatten (map build_flavor_reacts packet_flavors);;

(* These output relations have a "condensed" argument. That is, they are unary, 
   with a packet as the argument. Should only be done for certain built-ins. *)
let built_in_condensed_outrels = ["forward"] @ map (fun flav -> flavor_to_emitrelname flav) packet_flavors;;

(* All packet types must go here; 
   these are the tables that flag a rule as being "packet-triggered".*)
let built_in_packet_input_tables = map (fun flav -> flavor_to_inrelname flav) packet_flavors;;

let is_packet_in_table (relname: string): bool =
  mem relname built_in_packet_input_tables;;  

