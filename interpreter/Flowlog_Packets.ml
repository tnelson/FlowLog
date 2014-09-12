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
  (1A) header field defaults (if any)
  (2) marshal_packet
  (3) pkt_to_event *)


(*******************************************************************************
 *
 * (1) Generalized notion of a flavor of packet, like ARP or ICMP. Avoids ugly
 * multiple lists of "built-ins" where we can forget to add packet types, etc.
 *
 * label = string which is prepended to "_packet_in" and appended to "emit_" to
 *         arrive at relations.
 * superflavor = immediate supertype. If None, means that this is a direct
 *         subtype of an ethernet packet.
 * condition = a function that, given a variable name, constructs a formula that
 *         describes that it means to be a member of this flavor. E.g., to be an
 *         IP packet, you must have your dltyp field = 0x800.
 * fields = New fields added by this flavor. Must not be present in any
 *         superflavors.
 *
 ******************************************************************************)

type packet_flavor = {
  label: typeid;
  superflavor: typeid option;
  build_condition: (string -> formula);
  fields: (string * typeid) list
};;

let packet_flavors = [
   (* base type (Ethernet) *)
   {label = "packet"; superflavor = None;
    build_condition = (fun vname -> FTrue);
    fields = [("locsw", "switchid"); ("locpt", "portid"); ("dlsrc", "macaddr"); ("dldst", "macaddr"); ("dltyp", "ethtyp");
              ("dlvlan", "int")]};

   {label = "arp"; superflavor = Some "packet";
    build_condition = (fun vname -> FEquals(TField(vname, "dltyp"), TConst(hex_str_to_int_string "0x0806")));
    fields = [("arp_op", "int"); ("arp_spa", "ipaddr"); ("arp_sha", "macaddr"); ("arp_tpa", "ipaddr"); ("arp_tha", "macaddr")]};

   {label = "ip"; superflavor = Some "packet";
    build_condition = (fun vname -> FEquals(TField(vname, "dltyp"), TConst(hex_str_to_int_string "0x0800")));
    fields = [("nwsrc", "ipaddr"); ("nwdst", "ipaddr"); ("nwproto", "nwprotocol")]};  (* missing: frag, tos, chksum, ident, ...*)

  (* {label = "ipv6"; superflavor = Some "packet";
    build_condition = (fun vname -> FEquals(TField(vname, "dltyp"), TConst(hex_str_to_int_string "0x86DD")));
    fields = ["omgwtfbbq"]}; *)

  (* {label = "8021x"; superflavor = Some "packet";
    build_condition = (fun vname -> FEquals(TField(vname, "dltyp"), TConst(hex_str_to_int_string "0x888E")));
    fields = ["omgwtfbbq"]}; *)

  (* {label = "lldp"; superflavor = None;
    build_condition = (fun vname -> FEquals(TField(vname, "dltyp"), TConst(hex_str_to_int_string "0x88CC")));
    fields = ["omgwtfbbq"]}; *)

   {label = "tcp"; superflavor = Some "ip";
    build_condition = (fun vname -> FAnd(FEquals(TField(vname, "dltyp"), TConst(hex_str_to_int_string "0x0800")),
                                    FEquals(TField(vname, "nwproto"), TConst(hex_str_to_int_string "0x6"))));
    fields = [("tpsrc", "tpport"); ("tpdst", "tpport")]}; (* expect we'll want flags eventually *)

    (* "tpport" represents a transport-layer port, i.e. either TCP or UDP port. *)
   {label = "udp"; superflavor = Some "ip";
    build_condition = (fun vname -> FAnd(FEquals(TField(vname, "dltyp"), TConst(hex_str_to_int_string "0x0800")),
                                    FEquals(TField(vname, "nwproto"), TConst(hex_str_to_int_string "0x11"))));
    fields = [("tpsrc", "tpport"); ("tpdst", "tpport")]};

   {label = "igmp"; superflavor = Some "ip";
    build_condition = (fun vname -> FAnd(FEquals(TField(vname, "dltyp"), TConst(hex_str_to_int_string "0x0800")),
                                    FEquals(TField(vname, "nwproto"), TConst(hex_str_to_int_string "0x2"))));
    fields = [("igmp_ver_and_typ", "int"); ("igmp_addr", "ipaddr"); ("igmp_v3typ", "int")]};

   {label = "icmp"; superflavor = Some "ip";
    build_condition = (fun vname -> FAnd(FEquals(TField(vname, "dltyp"), TConst(hex_str_to_int_string "0x0800")),
                                    FEquals(TField(vname, "nwproto"), TConst(hex_str_to_int_string "0x1"))));
    fields = [("icmp_type", "int"); ("icmp_code", "int")]}; (* checksum will need calculation in runtime? *)

   {label = "mdns"; superflavor = Some "udp";
    build_condition = (fun vname -> FAnd(FEquals(TField(vname, "tpdst"), TConst("5353")),
                                         FAnd(FEquals(TField(vname, "nwproto"), TConst(hex_str_to_int_string "0x11")),
                                              FEquals(TField(vname, "dltyp"), TConst(hex_str_to_int_string "0x800")))));
    fields = [("mdns_question", "string")]};
  ];;


(*******************************************************************************
 *
 * TODO(adf): move this block to a "Flowlog_Events.ml" which also has the
 * bits from Flowlog_Types.ml
 *
 ******************************************************************************)

(* raises Not_found on invalid field, which is why we wrap with get_field *)
let get_field_helper (ev: event) (fldname: string): string  =
    StringMap.find fldname ev.values;;

(*  Should really be something more contentful than the empty string. *)
let field_is_defined (ev: event) (fldname: string): bool =
  (StringMap.mem fldname ev.values) && (get_field_helper ev fldname) <> "";;


(* (1A) Field defaults (if any)
   Just an association list from (type,field) -> value.
   Single place for defaults so that marshalling functions and verification can
   use a single source.

   Referer (get_field) is responsible for climbing the tree to get defaults of supertypes. *)
let defaults_table = [
  (("packet", "dlsrc"), "0");
  (("packet", "dldst"), "0");
  (("packet", "dlvlan"), "-1"); (* un-tagged *)
  (("ip_packet", "nwfrag"), "0");
  (("ip_packet", "nwttl"), "255");
  (("ip_packet", "nwtos"), "0");
  (("ip_packet", "nwchksum"), "0");
  (("ip_packet", "nwident"), "0");
  (("tcp_packet", "tpack"), "0");
  (("tcp_packet", "tpoffset"), "0");
  (("tcp_packet", "tpwindow"), "0");
  (("tcp_packet", "tpurgent"), "0");
];;


(**********************************************)
(**********************************************)
(**********************************************)

(* Fields that OpenFlow 1.0 permits modification of. *)
let legal_to_modify_packet_fields = ["locpt"; "dlsrc"; "dldst";
                                     "dlvlan"; "dlvlanpcp";
                                     "nwsrc"; "nwdst"; "nwtos";
                                     "tpsrc"; "tpdst"];;

let legal_to_match_packet_fields = ["dltyp"; "nwproto"; "locsw"]
                                   @ legal_to_modify_packet_fields;;

let swpt_fields = [("sw", "switchid");("pt", "portid")];;
let swdown_fields = [("sw", "switchid")];;
let flow_removed_fields = [
         ("inport",   "portid");
         ("sw",       "switchid");("reason",   "string");
         ("dlsrc",    "macaddr"); ("dldst",    "macaddr");
         ("dltyp",    "ethtyp");  ("nwproto",  "nwprotocol");
         ("tpsrc",    "tpport");  ("tpdst",    "tpport");
         ("nwsrcaddr","ipaddr");  ("nwdstaddr","ipaddr");
         ("nwsrcmask","int");     ("nwdstmask","int")];;

(**********************************************)

let flavor_to_typename (flav: packet_flavor): string = if flav.label = "packet" then "packet" else flav.label^"_packet";;
let flavor_to_inrelname (flav: packet_flavor): string = if flav.label = "packet" then "packet" else flav.label^"_packet";;
let flavor_to_cpinrelname (flav: packet_flavor): string = if flav.label = "packet" then "cp_packet" else "cp_"^flav.label^"_packet";;
let flavor_to_emitrelname (flav: packet_flavor): string = if flav.label = "packet" then "emit" else "emit_"^flav.label;;

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

let get_superflavor_typename (typename: string): typeid option =
  try
    match (StringMap.find typename map_from_typename_to_flavor).superflavor with
      | Some(l) -> Some(flavor_to_typename (StringMap.find l map_from_label_to_flavor))
      | None -> None
  with Not_found -> None;;

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

let flavor_to_field_decls (flav: packet_flavor): (string * typeid) list =
  let typename = flavor_to_typename flav in
  let supertypes = built_in_supertypes typename in
  let fieldslist = (fold_left (fun acc supertype -> (StringMap.find supertype map_from_typename_to_flavor).fields @ acc) [] supertypes) in
  let check_for_dupes = unique fieldslist in
  if (length fieldslist) <> (length check_for_dupes) then
    failwith ("Packet flavor "^flav.label^" had duplicate fieldnames when parent flavor fields were added.")
  else fieldslist;;

let flavor_to_fields (flav: packet_flavor): string list =
  map (fun (fname, _) -> fname) (flavor_to_field_decls flav);;

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

(*let create_id_assign (k: string): assignment = {afield=k; atupvar=k};;*)
let build_flavor_decls (flav: packet_flavor): sdecl list =
  [DeclInc(flavor_to_inrelname flav, flavor_to_typename flav);
   DeclInc(flavor_to_cpinrelname flav, flavor_to_typename flav);
   DeclEvent(flavor_to_typename flav, flavor_to_field_decls flav);
   DeclOut(flavor_to_emitrelname flav, FixedEvent(flavor_to_typename flav))];;
let build_flavor_reacts (flav: packet_flavor): sreactive list =
  [ReactInc(flavor_to_typename flav, IncDP, flavor_to_inrelname flav);
   ReactInc(flavor_to_typename flav, IncCP, flavor_to_cpinrelname flav);
   ReactOut(flavor_to_emitrelname flav, FixedEvent(flavor_to_typename flav),
            (*map create_id_assign (flavor_to_fields flav), *) OutEmit(flavor_to_typename flav))];;

let built_in_decls = [DeclInc(switch_reg_relname, "switch_port");
                      DeclInc(switch_down_relname, "switch_down");
                      DeclInc(startup_relname, "startup");
                      DeclInc("flow_removed", "flow_removed");
                      DeclOut("forward", SameAsOnFields);
                      DeclEvent("startup", []);
                      DeclEvent("switch_port", swpt_fields);
                      DeclEvent("switch_down", swdown_fields);
                      DeclEvent("flow_removed", flow_removed_fields)]
                    @ flatten (map build_flavor_decls packet_flavors);;

let built_in_reacts = [ ReactInc("switch_port", IncDP, switch_reg_relname);
                        ReactInc("switch_down", IncDP, switch_down_relname);
                        ReactInc("flow_removed", IncDP, "flow_removed");
                        ReactInc("startup", IncThrift, startup_relname);
                        ReactOut("forward", SameAsOnFields, OutForward);
                      ] @ flatten (map build_flavor_reacts packet_flavors);;

(* All packet types must go here;
   these are the tables that flag a rule as being "packet-triggered".*)
let built_in_packet_input_tables = map (fun flav -> flavor_to_inrelname flav) packet_flavors;;
let built_in_cp_packet_input_tables = map (fun flav -> flavor_to_cpinrelname flav) packet_flavors;;

let built_in_event_names = map (fun flav -> flavor_to_typename flav) packet_flavors;;

let is_built_in_packet_typename (tname: string) =
  mem tname built_in_event_names;;

let is_packet_in_table (relname: string): bool =
  mem relname built_in_packet_input_tables;;

let is_cp_packet_in_table (relname: string): bool =
  mem relname built_in_cp_packet_input_tables;;


(* Ascend up the flavor tree looking for a default. Start with the event's type. *)
let get_field (ev: event) (fldname: string): string =
  let supertypes = (built_in_supertypes ev.typeid) in

    let rec get_default_rec (typenamelist: typeid list) =
      match typenamelist with
        | typename::supertypes ->
          (try
              (*printf "get_default_rec %s %s %s %s\n%!" typename fldname (String.concat ", "supertypes) (assoc (typename,fldname) defaults_table);*)
              assoc (typename,fldname) defaults_table
           with Not_found -> get_default_rec supertypes)
        | _ -> raise (NoDefaultForField(fldname, ev.typeid)) in
    try
      let evval = get_field_helper ev fldname in
        if field_is_defined ev fldname then evval
        else get_default_rec supertypes
    with
      | Not_found -> get_default_rec supertypes;;


(*******************************************************************************
 *
 * (2) Functions to make packets from events.
 *
 * Each function returns the body payload of the enclosing packet (or simply
 * bytes in the case of Ethernet packets)
 *
 * If a function has a second argument, it is used for passing in the value
 * to be used for the body payload.
 *
 ******************************************************************************)

let make_eth (ev: event) (nw: Packet.nw): Packet.bytes =
  let dlSrc = macaddr_of_int_string (get_field ev "dlsrc") in
  let dlDst = macaddr_of_int_string (get_field ev "dldst") in
  let dlVlan_val = int_of_string (get_field ev "dlvlan") in
  let dlVlan = if dlVlan_val >= 0 then Some dlVlan_val else None in
    Packet.marshal {Packet.dlSrc = dlSrc; Packet.dlDst = dlDst;
                    Packet.dlVlan = dlVlan; Packet.dlVlanPcp = 0;
                    Packet.dlVlanDei = false; nw = nw };;

let make_arp (ev: event): Packet.nw =
  let arp_sha = macaddr_of_int_string (get_field ev "arp_sha") in
  let arp_spa = nwaddr_of_int_string (get_field ev "arp_spa") in
  let arp_tpa = nwaddr_of_int_string (get_field ev "arp_tpa") in
  let arp_op = int_of_string (get_field ev "arp_op") in
    match arp_op with
    | 1 -> (* request *)
        Arp(Arp.Query(arp_sha, arp_spa, arp_tpa))
    | 2 -> (* query *)
        let arp_tha = macaddr_of_int_string (get_field ev "arp_tha") in
                Arp(Arp.Reply(arp_sha, arp_spa, arp_tha, arp_tpa))
    | _ -> failwith "bad arp op";;

(* TODO(adf): add support for IP flags, stored as booleans by ocaml-packet *)
let make_ip (ev: event) (tp: Ip.tp): Packet.nw =
  let nwSrc = nwaddr_of_int_string (get_field ev "nwsrc") in
  let nwDst = nwaddr_of_int_string (get_field ev "nwdst") in
  let nwFrag = int_of_string (get_field ev "nwfrag") in
  let nwTTL = int_of_string (get_field ev "nwttl") in
  let nwTos = int_of_string (get_field ev "nwtos") in
  let nwChksum = int_of_string (get_field ev "nwchksum") in
  let nwIdent = int_of_string (get_field ev "nwident") in
  let nwOptions = Cstruct.create 0 in
  let nwFlags = { Packet.Ip.Flags.df = false;
                  Packet.Ip.Flags.mf = false} in
    Ip({src = nwSrc; dst = nwDst; flags = nwFlags; frag = nwFrag;
        tos = nwTos; ident = nwIdent; ttl = nwTTL; chksum = nwChksum;
        options = nwOptions; tp = tp});;

(* TODO(adf): add support for TCP flags, stored as booleans by ocaml-packet *)
let make_tcp (ev: event): Packet.Ip.tp =
  let tpSrc = int_of_string (get_field ev "tpsrc") in
  let tpDst = int_of_string (get_field ev "tpdst") in
  let tpSeq = nwaddr_of_int_string (get_field ev "tpseq") in
  let tpAck = nwaddr_of_int_string (get_field ev "tpack") in
  let tpOffset = int_of_string (get_field ev "tpoffset") in
  let tpWindow = int_of_string (get_field ev "tpwindow") in
  let tpUrgent = int_of_string (get_field ev "tpurgent") in
  let tpFlags =  { Tcp.Flags.ns = false; cwr = false; ece = false;
                   urg = false; ack = false; psh = false; rst = false;
                   syn = false; fin = false} in
    Tcp({Tcp.src = tpSrc; dst = tpDst; flags = tpFlags; seq = tpSeq;
         ack = tpAck; offset = tpOffset; window = tpWindow;
         chksum = 0; urgent = tpUrgent;
         (* empty payload *)
         payload = Cstruct.create 0});;

let make_udp (ev: event) (payload : Cstruct.t option) : Packet.Ip.tp =
  let tpSrc = int_of_string (get_field ev "tpsrc") in
  let tpDst = int_of_string (get_field ev "tpdst") in
  let payload = match payload with
                 |None -> Cstruct.create 0 (* empty payload *)
                 |Some payload -> payload in
    Udp({Udp.src = tpSrc; dst = tpDst; chksum = 0; payload = payload});;

let make_igmp2 (ev: event): Packet.Igmp.msg =
  let addr = nwaddr_of_int_string (get_field ev "igmp_addr") in
    Igmp.Igmp1and2({Igmp1and2.mrt = 0; chksum = 0; addr = addr})

let make_igmp3 (ev: event): Packet.Igmp.msg =
  let v3typ = int_of_string (get_field ev "igmp_v3typ") in
  let addr = nwaddr_of_int_string (get_field ev "igmp_addr") in
    Igmp.Igmp3({Igmp3.chksum = 0; grs =
                  [{Igmp3.GroupRec.typ = v3typ;
                    addr = addr; sources = []}]})

let make_igmp (ev: event): Packet.Ip.tp =
  let ver_and_typ = int_of_string (get_field ev "igmp_ver_and_typ") in
  let msg = if ver_and_typ = 0x22 then make_igmp3 ev else make_igmp2 ev in
    Igmp({Igmp.ver_and_typ = ver_and_typ; msg = msg})

let make_mdns (ev: event): Packet.Dns.t =
  let qname = get_field ev "mdns_question" in
    {Dns.id = 0; flags = 0;
     questions = [{Dns.Qd.name = qname; typ = 0x000c; class_ = 0x0001}];
     answers = []; authority = []; additional = [] }

(* Avoid gyrations with variant types by having these separate from the flavor declarations.
  Originally wanted each flavor to carry its own marshal function, but due to different Packet
  module types + heterogenous lists, it would have been involved.
  Probably a better way to do this. Kludging for now. *)

let generic_eth_body (ev: event): Packet.nw =
  let dlTyp = int_of_string (get_field ev "dltyp") in
    Packet.Unparsable(dlTyp, Cstruct.create(0));;

let generic_ip_body (ev: event): Packet.Ip.tp =
  let nwProto = int_of_string (get_field ev "nwproto") in
    Packet.Ip.Unparsable(nwProto, Cstruct.create(0));;

let marshal_packet (ev: event): Packet.bytes =
  match ev.typeid with
    | "packet" -> make_eth ev (generic_eth_body ev)
    | "ip_packet" -> make_eth ev (make_ip ev (generic_ip_body ev))
    | "arp_packet" -> make_eth ev (make_arp ev)
    | "icmp_packet" -> failwith "icmp unsupported"
    | "igmp_packet" -> make_eth ev (make_ip ev (make_igmp ev))
    | "tcp_packet" -> make_eth ev (make_ip ev (make_tcp ev))
    | "udp_packet" -> make_eth ev (make_ip ev (make_udp ev None))
    | "mdns_packet" -> make_eth ev (make_ip ev (make_udp ev (Some(
                          Dns.serialize (make_mdns ev)))))
    | _ -> failwith ("marshal_packet: unknown type: "^ev.typeid);;


(*******************************************************************************
 *
 * (3) Functions to make events from packets
 *
 * Each function receives a packet and returns a list of (key, value) pairs
 * which are the event fields from this packet flavour.
 *
 ******************************************************************************)

let get_arp (pkt: Packet.packet): (string*string) list =
  match pkt.nw with
    | Arp(Arp.Query(arp_sha, arp_spa, arp_tpa)) ->
      [("arp_sha", macaddr_to_int_string arp_sha);
       ("arp_spa", nwaddr_to_int_string arp_spa);
       ("arp_tha", "0");
       ("arp_tpa", nwaddr_to_int_string arp_tpa);
       ("arp_op", "1")]
    | Arp(Arp.Reply(arp_sha, arp_spa, arp_tha, arp_tpa)) ->
      [("arp_sha", macaddr_to_int_string arp_sha);
       ("arp_spa", nwaddr_to_int_string arp_spa);
       ("arp_tha", macaddr_to_int_string arp_tha);
       ("arp_tpa", nwaddr_to_int_string arp_tpa);
       ("arp_op", "2")];
    | _ -> [];;

let get_ip (pkt: Packet.packet): (string*string) list =
  match pkt.nw with
   | Ip(_) ->
    [("nwsrc", nwaddr_to_int_string (Packet.nwSrc pkt));
     ("nwdst", nwaddr_to_int_string (Packet.nwDst pkt));
     ("nwproto", string_of_int (Packet.nwProto pkt))]
   | _ -> [];;

let get_tcp (pkt: Packet.packet): (string*string) list =
  match pkt.nw with
   | Ip(ip_pkt) -> (match ip_pkt.tp with
      | Tcp(_) ->
        [("tpsrc", string_of_int (Packet.tpSrc pkt));
         ("tpdst", string_of_int (Packet.tpDst pkt))]
      | _ -> [])
   | _ -> [];;

let get_udp (pkt: Packet.packet): (string*string) list =
  match pkt.nw with
   | Ip(ip_pkt) -> (match ip_pkt.tp with
      | Udp(_) ->
        [("tpsrc", string_of_int (Packet.tpSrc pkt));
         ("tpdst", string_of_int (Packet.tpDst pkt))]
      | _ -> [])
   | _ -> [];;

let get_igmp_helper (igmp_pkt: Packet.Igmp.t): (string*string) list =
  match igmp_pkt.Igmp.msg with
    | Igmp.Igmp1and2(i12) ->
      [("igmp_addr", nwaddr_to_int_string (i12.Igmp1and2.addr));
       ("igmp_v3typ", "0")]
    | Igmp.Igmp3(i3) -> (let gr = hd i3.Igmp3.grs in
      [("igmp_addr", nwaddr_to_int_string (gr.Igmp3.GroupRec.addr));
       ("igmp_v3typ",(string_of_int (gr.Igmp3.GroupRec.typ)))])
    | _ -> []

let get_igmp (pkt: Packet.packet): (string*string) list =
  match pkt.nw with
   | Ip(ip_pkt) -> (match ip_pkt.tp with
      | Igmp(igmp_pkt) ->
        [("igmp_ver_and_typ", string_of_int (igmp_pkt.Igmp.ver_and_typ))]
        @ (get_igmp_helper igmp_pkt)
      | _ -> [])
   | _ -> [];;

let pkt_to_event (sw : switchId) (pt: port) (pkt : Packet.packet) : event =
   let vlan_val = match pkt.Packet.dlVlan with | None -> "-1" | Some(x) -> string_of_int x in
   let values = [
    ("locsw", Int64.to_string sw);
    ("locpt", NetCore_Pretty.string_of_port pt);
    ("dlsrc", macaddr_to_int_string pkt.Packet.dlSrc);
    ("dldst", macaddr_to_int_string pkt.Packet.dlDst);
    ("dlvlan", vlan_val);
    ("dltyp", string_of_int (Packet.dlTyp pkt))]
    @ (get_arp pkt)
    @ (get_ip pkt)
    @ (get_tcp pkt)
    @ (get_udp pkt)
    @ (get_igmp pkt) in
    let typeid = (match (Packet.dlTyp pkt) with
      | 0x0806 -> "arp_packet"
      | 0x0800 -> (match (Packet.nwProto pkt) with
                    | 0x02 -> "igmp_packet"
                    | 0x06 -> "tcp_packet"
                    | 0x11 -> "udp_packet"
                    | _ -> "ip_packet")
      | _ -> "packet") in
    {typeid = typeid; values = construct_map values};;


(* removes the packet_in atom (since that's meaningless here).
   returns the var the old packet was bound to, and the trimmed fmla

   IMPORTANT: This function must remove cp_<x>_packet, not just <x>_packet.
   Otherwise the Prolog program produced will be incorrect for CP-packet triggered rules
   (it will have __locsw instead of p__locsw) *)
let rec trim_packet_from_body (body: formula): (string * string * formula) =
  match body with
    | FTrue -> ("", "", body)
    | FFalse -> ("", "", body)
    | FEquals(t1, t2) -> ("", "", body)
    | FAnd(f1, f2) ->
      let (rel1, var1, trimmed1) = trim_packet_from_body f1 in
      let (rel2, var2, trimmed2) = trim_packet_from_body f2 in
      let trimmed = if trimmed1 = FTrue then
                      trimmed2
                    else if trimmed2 = FTrue then
                      trimmed1
                    else
                      FAnd(trimmed1, trimmed2) in
      if (var1 = var2) || var1 = "" then
        (rel2, var2, trimmed)
      else if var2 = "" then
        (rel1, var1, trimmed)
      else failwith ("trim_packet_from_clause: multiple variables used in packet_in: "^var1^" and "^var2)
    | FNot(f) ->
      let (r, v, t) = trim_packet_from_body f in
        (r, v, FNot(t))
    | FOr(f1, f2) -> failwith "trim_packet_from_clause"
    (* Don't remove non-packet input tables. Those flag caller that the clause is not packet-triggered *)
    | FAtom("", relname, [TVar(varstr)]) when (is_packet_in_table relname) || (is_cp_packet_in_table relname) ->
      (relname, varstr, FTrue)
    | _ -> ("", "", body);;

(***************************************************************************)
(* Use reactives to look up appropriate output relation for this event/src *)
let inc_event_to_relname (p: flowlog_program) (typename: string) (from: eventsource): string =
    Hashtbl.find p.memos.incomingmap (typename, from);;
let inc_event_to_relnames (p: flowlog_program) (notif: event) (from: eventsource): string list =
  let supertypes = (built_in_supertypes notif.typeid) in
    map (fun stype -> inc_event_to_relname p stype from) supertypes;;

