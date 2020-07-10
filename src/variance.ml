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
  		| _ -> v

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
		| _ -> v

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
		| (v1, v2) when (in_additive v1) && (in_additive v2) -> inter v1 v2
		| (v1, v2) when (in_additive (not v1)) && (in_additive v2) -> not (inter (not v1) v2)
		| (v1, v2) when (in_additive v1) && (in_additive (not v2)) -> not (inter (dual v1) (not v2))
		| (v1, v2) when (in_additive (not v1)) && (in_additive (not v2)) -> inter (dual (not v1)) (not v2))
		| (Monotone, _) -> v2
		| (Antitone, _) -> (not v2)
		| () 


(* returns Meet o variance *)
let composition_meet  = composition Meet

(*
let compute_variance (f : formula) : variance =
	return Any;*)