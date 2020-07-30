let rec cons_list_list (a : 'a) (alistlist : 'a list list) : 'a list list = 
	List.map (fun l -> a::l) alistlist
	
let rec permutation_pairs (l1 : 'a list) (l2 : 'a list) : ('a * 'a) list =
	match (l1,l2) with
		| [],_ | _, [] -> []
		| (e1::q1, e2::q2) -> (e1,e2)::(permutation_pairs [e1] q2 @ permutation_pairs q1 l2)