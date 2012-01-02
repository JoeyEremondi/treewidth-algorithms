open Str

module Graph =
  struct
    type graph = {mutable n:int; mutable m:int; mutable e:int list array}
    let empty = {n=0; m=0; e=[||]}
    
    let neighborhood g u =
      g.e.(u)

    let read_dgf file = 
      let g = empty in
      let chan = open_in file in
      try
        while true; do
	  let rawline = input_line chan in
          let words = split (regexp " ") rawline in
          match words with
            | ["p";"edge";w1;w2] -> let n = int_of_string w1 in
				    let m = int_of_string w2 in
				    g.n <- n;
				    g.m <- m;
				    g.e <- Array.make n []
            | [p;w1;w2] -> (match p with
                | "c" | "n" -> ()
                | "e" -> (try let n1 = (int_of_string w1)-1 in 
			      let n2 = (int_of_string w2)-1 in
                              if n1 >= g.n || n2 >= g.n then
				()
			      else (g.e.(n1) <- n2::g.e.(n1);
				    g.e.(n2) <- n1::g.e.(n2))
                  with x -> raise x)
                | x -> ())
            | _ -> ()
        done; failwith "This won't happen"
      with End_of_file ->
        close_in chan;
        g
  end;;
