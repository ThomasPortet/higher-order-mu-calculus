let rec cons_list_list (a : 'a) (alistlist : 'a list list) : 'a list list = 
	match alistlist with
        | [] -> []
        | hd::q -> (a::hd)::(cons_list_list a q)