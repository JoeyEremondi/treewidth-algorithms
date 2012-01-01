open Graph
open Printf
open Scanf

module Hypergraph = 
struct
  type hypergraph = {
    n: int; (* number of hyper vertices *)
    m: int; (* number of hyper edges *)
    he: int list list; (* hyper edges *)
    hv2e: int -> int*int; (* map hyper vertices to edges *)
    e2hv: int*int -> int  (* map edges to hyper vertices *)
  }
      
  (**
   * fun graph -> hypergraph
   * hyper vertices are edges of g
   * hyper edges are sets of edges with a common endpoint
   *)  
  let from_graph g =
    let e2hv_map = Hashtbl.create g.Graph.m in
    let hv2e_map = Hashtbl.create g.Graph.m in
    let n = ref 0 in
    
    (**
     * int list -> int list
     * for each vertex u, we build an hyper edge which connect all edges from u
     *)
    let build_hg u l =
      let bind_hv v =
        let edge = if u < v then (u,v) else (v,u) in
        try
          let hv = Hashtbl.find e2hv_map edge in
          hv
        with | Not_found -> begin
          let hv = !n in
          Hashtbl.add e2hv_map edge hv;
          Hashtbl.add hv2e_map hv edge;
          incr n;
          hv
        end
      in
      List.map bind_hv l
    in
    let he = Array.mapi build_hg (g.Graph.e) in
    {
      n = !n;
      m = g.Graph.n;
      he = Array.to_list he;
      hv2e = (fun hv -> Hashtbl.find hv2e_map hv);
      e2hv = fun e -> Hashtbl.find e2hv_map e
    }
      
  (**
   * hypergraph -> (int*int) option
   * return the hyper vertex if h is reduced to an unique edge
   *) 
  let get_sole_vertex h =
    if h.n == 1 then
      Some (h.hv2e 0)
    else
      None
  
  (**
   * hypergraph -> filename
   * write h in a.hgr format
   *)
  let to_hgr h =
    let name = "hypergraph.hgr" in
    let out = open_out name in
    fprintf out "%d %d\n" h.m h.n;
    List.iter ( fun l ->
      List.iter ( fun hv -> fprintf out "%d " (hv+1) ) l;
      output_char out '\n'
    ) h.he;
    close_out out;
    name
      
  (**
   * string -> int -> in_chan
   * system call to hmetis
   *)
  let callhmetis hgr ubFactor =
    let _ = Unix.system ("./shmetis "^hgr^" 2 "^(string_of_int ubFactor)^" > /dev/null") in
    ( open_in (hgr^".part.2") )
      
  (**
   * hypergraph -> int -> (hypergraph * hypergraph)
   * partition h in two hypergraph via hmetis
   *)
  let partition h ubFactor =
    let nl = ref 0
    and nr = ref 0
    and e2hv_mapl = Hashtbl.create 1
    and e2hv_mapr = Hashtbl.create 1
    and hv2e_mapl = Hashtbl.create 1
    and hv2e_mapr = Hashtbl.create 1 in
    
    (** build map from hyper parts to std graph **)
    let build_maps i part =
      let edge = h.hv2e i in
      if part=1 then begin
        let hv = !nl in
        Hashtbl.add hv2e_mapl hv edge;
        Hashtbl.add e2hv_mapl edge hv;
        incr nl
      end
      else begin
        let hv = !nr in
        Hashtbl.add hv2e_mapr hv edge;
        Hashtbl.add e2hv_mapr edge hv;
        incr nr
      end        
    in
    
    let hgr = to_hgr h in
    let partition_file = callhmetis hgr ubFactor in
    
    (* scan the file containing the partition output by hmetis *)
    for i=0 to (h.n - 1) do
      let part = fscanf partition_file "%d\n" (fun n -> n) in
      build_maps i part
    done;
    close_in partition_file;
    
    let hv2e_l = (fun hv -> Hashtbl.find hv2e_mapl hv)
    and hv2e_r = (fun hv -> Hashtbl.find hv2e_mapr hv)
    and e2hv_l = (fun e  -> Hashtbl.find e2hv_mapr e)
    and e2hv_r = (fun e  -> Hashtbl.find e2hv_mapr e)
    and he_r = ref []
    and he_l = ref [] in
    
    (** build hyper edges of partitions **)
    let build_parts he =
      (** return part of he if all v are in the same part, -1 otherwise **)
      let part_of_he = List.fold_left (fun r part -> begin
          if r = -2 then part (* init *)
          else if r = part then part else -1
        end ) (-2) he in
      
      if part_of_he = 0 then begin
        let renamed_he = List.map (fun u -> e2hv_r (h.hv2e u) ) he in
        he_r := renamed_he :: !he_r
      end
      else if part_of_he = 1 then begin
        let renamed_he = List.map (fun u -> e2hv_l (h.hv2e u) ) he in
        he_l := renamed_he :: !he_l
      end      
    in
        
    List.iter build_parts h.he;
    
    let l = {
      n= !nl;
      m= List.length !he_l;
      he= !he_l;
      hv2e= hv2e_l;
      e2hv= e2hv_l }
    and r = {
      n= !nr;
      m= List.length !he_l; 
      he= !he_r;
      hv2e= hv2e_r;
      e2hv= e2hv_r } in
      
    (l,r)
end;;

