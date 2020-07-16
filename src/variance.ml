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

let not (v : variance) : variance =
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
		| Join | Meet | Additive -> true
		| _ -> false

let in_Nadditive (v : variance) : bool =
	match v with
		| NJoin | NMeet | NAdditive -> true
		| _ -> false

(* should only be called between two variances either in {Join ; Meet ; Additive} or in {NJoin ; NMeet ; Nadditive}
otherwise the intersection between two random variances isn't properly defined 
and we return Any arbitrarily or their value if they're equal *)
let inter (v1 : variance) (v2 : variance) : variance =
	match (v1, v2) with
		| (v1,v2) when v1 == v2 -> v1
		| (v1,v2) when (in_additive v1) && (in_additive v2) -> Additive
		| (v1,v2) when (in_Nadditive v1) && (in_Nadditive v2) -> NAdditive
		| (_,_) -> Any


let composition (v1 : variance) (v2 : variance) : variance =
	match (v1,v2) with 
		| (None, _) |(_, None) -> None
		| (Any, _) |(_, Any) -> Any
		| (Monotone, _) -> v2
		| (_, Monotone) -> v1 (* ? ? *)
		| (Antitone, _) -> (not v2) 
		| (_, Antitone) -> (not v1) (* ? ? *)
		| (v1, v2) when (in_additive v1) && (in_additive v2) -> inter v1 v2
		| (v1, v2) when (in_additive (not v1)) && (in_additive v2) -> not (inter (not v1) v2)
		| (v1, v2) when (in_additive v1) && (in_additive (not v2)) -> not (inter (dual v1) (not v2))
		| (v1, v2) when (in_additive (not v1)) && (in_additive (not v2)) -> inter (dual (not v1)) (not v2)
		| _ -> Any (* this case should never occur *) 


(* returns Meet o variance *)
let composition_meet  = composition Meet

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
    | va::l -> {variable = va.variable ; variance = not va.variance}::negated_variances l 

(* returns a list of variance assignments needed 
or None if there are no possible variance assignments of the variables to satisfy the formula's type *)
let variances_needed (f : formula) : (variance_assignment list) option =

let rec tc (form : formula) (l : variance_assignment list) : (variance_assignment list) option =

	match form with 
		| True -> Some(l)
 		| Bottom -> Some(negated_variances l)
 		| Neg(f) -> tc f (negated_variances l)
		| Mu(x,tau,f) -> (match (assign_variance {variable = x ; variance = Monotone} l) with 
							| Some (variance_assignment_list) -> tc f variance_assignment_list
							| None -> None )(* the variance couldn't be assigned because it wasn't compatible with the previous one *)
		| Nu(x,tau,f) -> (match (assign_variance {variable = x ; variance = Antitone} l) with 
							| Some (variance_assignment_list) -> tc f variance_assignment_list
							| None -> None )(* the variance couldn't be assigned because it wasn't compatible with the previous one *)
		| _ -> Some(l) 
	in
	tc f []


