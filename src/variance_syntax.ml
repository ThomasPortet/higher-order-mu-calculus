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
  