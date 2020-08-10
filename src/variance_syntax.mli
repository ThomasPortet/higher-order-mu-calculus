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

val variance_from_string : string -> variance

val v_to_string : variance -> string

val va_to_string : variance_assignment -> string

val val_to_string : variance_assignment list -> string

val all_variances : variance list