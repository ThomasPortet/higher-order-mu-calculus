type var = string

type variance  =
  | None
  | Any
  | Monotone
  | Join
  | Meet
  | Additive
  (* duals of previous variances*)
  | Antitone
  | NJoin
  | NMeet
  | NAdditive


type variance_assignment = {
  variable : var;
  variance : variance
  }

let variance_from_string (s : string) : variance = 
  match s with 
    | s when (String.equal (String.lowercase_ascii s) "none") -> None
    | s when (String.equal (String.lowercase_ascii s) "monotone") -> Monotone
    | s when (String.equal (String.lowercase_ascii s) "antitone") -> Antitone
    | s when (String.equal (String.lowercase_ascii s) "join") -> Join
    | s when (String.equal (String.lowercase_ascii s) "meet") -> Meet
    | s when (String.equal (String.lowercase_ascii s) "njoin") -> NJoin
    | s when (String.equal (String.lowercase_ascii s) "nmeet") -> NMeet
    | s when (String.equal (String.lowercase_ascii s) "additive") -> Additive
    | s when (String.equal (String.lowercase_ascii s) "nadditive") -> NAdditive
    | s when (String.equal (String.lowercase_ascii s) "any") -> Any
    | _ -> failwith ("non-recognised input variance : "^s)

let v_to_string (v : variance) : string = 
  match v with
    | None -> "None"
    | Any -> "Any"
    | Monotone -> "Monotone"
    | Join -> "Join"
    | Meet -> "Meet"
    | Additive -> "Additive"
    | Antitone -> "Antitone"
    | NJoin -> "NJoin"
    | NMeet -> "NMeet"
    | NAdditive -> "NAdditive"


let va_to_string (va : variance_assignment) : string =
  va.variable^" : "^(v_to_string va.variance)

let rec val_to_string (l : variance_assignment list) : string =
   match l with
    | [] -> ""
    | va::l -> va_to_string va ^ "\n" ^ val_to_string l

let all_variances : variance list = 
  [None ;NAdditive; Additive; NJoin;Any ;NMeet; Join; Meet; Monotone; Antitone]