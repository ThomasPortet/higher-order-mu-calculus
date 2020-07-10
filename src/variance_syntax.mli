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
  
val variance_from_string : string -> variance
val to_string : variance -> string