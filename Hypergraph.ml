(*open Array*)
open Graph

module Hypergraph = 
  struct
    type hypergraph = H of int * int list list * ((int -> int * int) * (int * int -> int))

    let id x = x
    let from_graph g = 
      let m = to_matrix g in
      let cur=ref 0 in
      for i=0 to (Array.length m) - 1 do
	for j=i to (Array.length m) - 1 do
	  if m.(i).(j) == 1 then begin
	    m.(i).(j) <- !cur;
	    m.(j).(i) <- !cur;
	    incr cur;
	  end;
	done;
      done;
      let ne = !cur in
      let hedges = ref [] in
      for i=0 to (Array.length m) - 1 do
	let tmp = ref [] in
	for j=0 to (Array.length m) - 1 do
	  if m.(i).(j) != 0 then begin
	    tmp := (m.(i).(j)) :: (!tmp);
	  end;
	done;
	hedges := (!tmp) :: (!hedges);
      done
      let from (a,b) = m.(a).(b) in
      let to_original n = 
	let res = ref 0 in
	for i=0 to (Array.length m) - 1 do
	  for j=i to (Array.length m) - 1 do
	    if m.(i).(j) == n then 
	      res := (i,j);
	  done;
	done;
	!res;
      in H (ne,hedges,(to_original,from))
      
      let get_sole_vertex H(e,(to_original,_)) = 
	if Array.length e == 1 then
	  Some (to_original 0)
	else 
	  None 
	    
    let partition h = 
      let (h1,h2) = hmetis h in
      (h1,h2)

(*     let hmetis h =  *)
(*       let h = H(e,(to,from)) in *)
(*       let hhgr = tohgr h in *)
(*       let res = callhmetis hhgr in *)
(* 	for i = 0 to (Array.length res) - 1 do *)
(* 	  if res.(i) == 0  *)
(* 	  then begin *)
	    
(* 	  end *)
(* 	  else begin *)

(* 	  end *)
(* 	done; *)

      
    let minimise h =
      let find x v = 
	let i = ref 0 in
	  while (!i < Array.length v && v.(!i) != x) do
	    incr i;
	  done;
	if (!i == Array.length v) 
	then -1 
	else !i 
    let f res e = if List.mem e res then res else e::res
    and unique = List.fold_left f [] in
    let H(n,hedges,(to_original,from_original)) = h in
    let mapp = Array.of_list (unique (List.flatten hedges)) in
    let toto i = to_original (mapp.(i))
    and fromfrom (a,b) = find (from_original (a,b)) mapp in
    H (n,List.map (fun l -> List.map (fun x -> find x mapp)) hedges,toto,fromfrom)

  end
