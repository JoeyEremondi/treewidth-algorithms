open Graph
open Printf

exception StopThisShitBeforeItsTooLate of string

module Hypergraph = 
struct
  type hypergraph = {
    n: int; (* number of hyper vertices *)
    m: int; (* number of hyper edges *)
    he: int list array; (* hyper edges *)
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
      he = he;
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
  let _to_hgr h =
    let name = "hypergraph.hgr" in
    let out = open_out name in
    fprintf out "%d %d\n" h.m h.n;
    Array.iter ( fun l ->
      List.iter ( fun hv -> fprintf out "%d " (hv+1) ) l;
      output_char out '\n'
    ) h.he;
    close_out out;
    name
      
  (**
     * filein -> fileout
     * system call to hmetis
  *)
  let callhmetis hgr n =
    match (Unix.system ("./shmetis "^hgr^" 2 1")) with
      | 0 -> let fin = open_in (hgr^".part.2") in
	     let part = Array.make n 0 in
	     for i=0 to (n-1) do
	       part.(i) <- fscanf fin "%d\n" (fun n -> n)
	     done;
	     part
      | _ -> failwith "OLOLOLOL"
      
  (** TODO
      * hypergraph -> (hypergraph * hypergraph)
      * partition h in two hypergraph via hmetis
  *)
  let partition h =
    let hgr = _to_hgr h in
    let n = h.n
    let partition_array = callhmetis hgr n in
    
    
      

(*    let minimise h =                                                                    *)
(*      let find x v =                                                                    *)
(*        let i = ref 0 in                                                                *)
(*        while (!i < Array.length v && v.(!i) != x) do                                   *)
(*          incr i                                                                        *)
(*        done;                                                                           *)
(*        if (!i == Array.length v)                                                       *)
(*        then -1                                                                         *)
(*        else !i in                                                                      *)
(*      let f res e = if List.mem e res then res else e::res in                           *)
(*      let  unique = List.fold_left f [] in                                              *)
(*      let H(n,hedges,(to_original,from_original)) = h in                                *)
(*      let mapp = Array.of_list (unique (List.flatten hedges)) in                        *)
(*      let toto i = to_original (mapp.(i))                                               *)
(*      and fromfrom (a,b) = find (from_original (a,b)) mapp in                           *)
(*      H (n,List.map (fun l -> List.map (fun x -> find x mapp) l) hedges,(toto,fromfrom))*)

end
