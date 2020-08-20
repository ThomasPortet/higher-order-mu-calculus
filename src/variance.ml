open Variance_syntax
open Mu_calculus_syntax

(* returns true if v1 is smaller or equal than v2 *)
let rec smaller (v1 : variance) (v2 : variance) : bool =
	match (v1,v2) with
		| (v1,v2) when v1 == v2 -> true 
		| (Any, _) -> true
		| (_, None) -> true
		| (Monotone, v2) -> if (v2 == Join || v2 == Meet) then true else (smaller Join v2)
		| (Antitone, v2)  -> if (v2 == NJoin || v2 == NMeet) then true else (smaller NJoin v2)
		| (v1, Additive) when (v1 == Join || v1 == Meet) -> true
		| (v1, NAdditive) when (v1 == NJoin || v1 == NMeet) -> true
		| (_,_) -> false

let dual (v : variance) : variance =
	match v with	
 		| Join -> Meet
  		| Meet -> Join
  		| NJoin -> NMeet
  		| NMeet -> NJoin
  		| _ -> v (* ? undefined cases ? *)

let not_v (v : variance) : variance =
	match v with
		| Monotone -> Antitone
		| Antitone -> Monotone
		| Join -> NJoin
		| NJoin -> Join
		| Meet -> NMeet
		| NMeet -> Meet
		| Additive -> NAdditive
		| NAdditive -> Additive
		| _ -> v (* ? undefined cases ? *)

let in_additive (v : variance) : bool =
	match v with
		| Monotone | Join | Meet | Additive -> true
		| _ -> false

let in_Nadditive (v : variance) : bool =
	in_additive (not_v v)

(* should only be called between two variances in {Monotone; Join ; Meet ; Additive}
@raise Failure if v1 ^ v2 isn't properly defined *)
let inter (v1 : variance) (v2 : variance) : variance =
	match (v1, v2) with
		| (v1,v2) when v1 == v2 -> v1
		| (Additive, v) | (v, Additive) -> v
		| (v1,v2) when not ((in_additive v1) && (in_additive v2)) -> failwith ("Error in variances intersection : "^(v_to_string v1)^" ^ "^(v_to_string v2)^" is not defined.")
		| _,_ -> Monotone 
(* 
@raise Failure if v1 o v2 isn't properly defined *)
let composition (v1 : variance) (v2 : variance) : variance =
	match (v1,v2) with 
		| (None, _) |(_, None) -> None
		| (Any, _) |(_, Any) -> Any
		| (v1, v2) when (in_additive v1) && (in_additive v2) -> inter v1 v2
		| (v1, v2) when (in_additive (not_v v1)) && (in_additive v2) -> not_v  (inter (not_v  v1) v2)
		| (v1, v2) when (in_additive v1) && (in_additive (not_v  v2)) -> not_v  (inter (dual v1) (not_v  v2))
		| (v1, v2) when (in_additive (not_v  v1)) && (in_additive (not_v  v2)) -> inter (dual (not_v  v1)) (not_v  v2)
		| _ ->  failwith ("Error in variances composition : this case should never occur !")

(* returns Meet o variance *)
let composition_meet  = composition Meet

(* returns v1^-1 o v2, in other words all the variances v in V | v2 = v1 o v *)

let inverse_variance_composition  (v1 : variance) (v2 : variance) : variance list =
	let rec test (vl : variance list) : variance list =
		match vl with
			| [] -> []
			| v::l when (composition v1 v) == v2 -> v::(test l)
			| v::l -> test l
	in test all_variances

(* returns a list containing all variances bigger than v *)
let bigger_variances (v : variance) :variance list =
	let rec test (vl : variance list) : variance list =
		match vl with
			| [] -> []
			| v1::l when (smaller v v1) -> v1::(test l)
			| v1::l -> test l
	in test all_variances

let greatest_smaller_variances  (v1 : variance) (v2 : variance) : variance =
	let rec test (vl : variance list) : variance =
		match vl with
			| [] -> Any
			| v::l when (smaller v v1) && (smaller v v2) -> v
			| v::l -> test l
	in test all_variances

let inverse_meet = inverse_variance_composition Meet

let rec assignment (x : var) (l : variance_assignment list) : variance option =
  match l with
    | [] -> None
    | va::l when (String.equal va.variable x) -> Some(va.variance)
    | _::l ->  assignment x l

let rec replace_variance_assignment (va : variance_assignment) (l : variance_assignment list) : variance_assignment list =
  match l with 
    | [] -> [va]
    | previous_assignment::l when (String.equal va.variable previous_assignment.variable) -> va::l
    | _::l ->  replace_variance_assignment va l

(* inverse the assignments presents in l in val2*)
let rec inverse_assignments (inverse_l : variance_assignment list) (val2 : variance_assignment list) : variance_assignment list =
  match inverse_l with 
    | [] -> val2
    | va::l -> let x = va.variable in match (assignment x val2) with
    				| Some(v) -> inverse_assignments l (replace_variance_assignment {variable = x ; variance = not_v v} val2)
    				| None -> inverse_assignments l val2

(* returns an optional : a list of variance assignements if the variable has been assigned the desired variance,
returns None if it didn't because a previous variance assignment of the variable isn't compatible with the new one *)
let assign_variance (va : variance_assignment) (l : variance_assignment list) : variance_assignment list option =
  match (assignment va.variable l) with
    | None -> Some (va::l)
    | Some(previous_variance) when (smaller previous_variance va.variance) -> Some((replace_variance_assignment va l))
    | _ -> None (* in this case, we cannot add the desired variance assignment *)

let rec negated_variances (l : variance_assignment list) : variance_assignment list =
  match l with 
    | [] -> []
    | va::l1 -> {variable = va.variable ; variance = not_v va.variance}::negated_variances l1 

let rec merge_assignment_lists (l1 : variance_assignment list) (l2 : variance_assignment list) : variance_assignment list =
	match l1 with 
		| [] -> l2
		| va::l -> match assignment va.variable l2 with
						| Some(v) -> merge_assignment_lists l (replace_variance_assignment {variable = va.variable; variance = greatest_smaller_variances v va.variance} l2)
						| None -> merge_assignment_lists l (va::l2)

(* returns a list of variance assignments needed 
or None if there are no possible variance assignments of the variables to satisfy the formula's type *)
let variances_needed (f : formula) : (variance_assignment list) option  =
	let rec tc (form : formula) (l : variance_assignment list) : (variance_assignment list) option =

	match form with 
		| Top -> Some(l) 
 		| Neg(phi) -> let notl = negated_variances l in 
 						(match (tc phi notl) with
							| Some (new_l) ->  Some( inverse_assignments l (negated_variances new_l))
 							| None -> failwith ("Error in variances computation : the formula is ill-typed"))

		| Mu(x,tau,phi) -> (match assign_variance {variable = x ; variance = Monotone} l with 
							| Some (vl) -> tc phi vl
							| None -> failwith ("Error in variances computation : the formula is ill-typed")) (* the variance couldn't be assigned because it wasn't compatible with the previous one *)
		| PreVariable (x) -> (match (assignment x l) with
									| Some (v) ->  Some(l)
									| _ -> Some(replace_variance_assignment {variable = x ; variance = Any} l) )			
		| And (phi,psi) -> let sl1 = (tc phi l) in
						   let sl2 = (tc psi l) in
						   let s =
						   (match (sl1,sl2) with
							| (Some(l1), Some(l2)) -> (*let () = print_string ("l1 : "^(val_to_string l1)^"l2 : "^(val_to_string l2)^"\n") in*) Some(merge_assignment_lists l1 l2) 
							| (Some(l1), None) -> Some(l1)
							| (_, _) -> sl2)in (match s with
														| Some(a) -> (*let() = print_string ((f_to_string form)^(val_to_string a)) in*) Some(a)
														| None -> None)
		| Diamond (a,phi) -> tc phi l
		| Lambda (x,phi) -> (match assign_variance {variable = x ; variance = Any} l with 
							| Some (vl) -> let s = tc phi vl in (match s with
														| Some(a) -> (*let() = print_string ("Lambda : "^(val_to_string a)) in*) Some(a)
														| None -> None)
							| None -> failwith "Error in variances computation : the formula is ill-typed" )
		| Application (f, phi) -> let sl1 = (tc f l) in match sl1 with 
									| Some(l1) ->  (match (tc phi l1) with
														| Some(a) -> (*let() = print_string ("Applic : "^(f_to_string f)^"a1 : "^(val_to_string l1)^"a2 : "^(val_to_string a)^"\n") in*) Some(a)
														| None -> None)
									| None -> tc phi l
						   
	in
	(tc f [])


let print_compute_variance (f : formula) : unit =
  let vl = variances_needed f in 
  let s = match vl with 
    | Some (l) -> val_to_string l
    | None -> "no variances needed"
  in 
  let () = print_string (s^"\n") in ()