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


let variance_from_string (s : string) : variance = 
  if (String.equal (String.lowercase_ascii s) "none") then None else
  if (String.equal (String.lowercase_ascii s) "monotone") then Monotone else
  if (String.equal (String.lowercase_ascii s) "antitone") then Antitone else
  if (String.equal (String.lowercase_ascii s) "join") then Join else
  if (String.equal (String.lowercase_ascii s) "meet") then Meet else
  if (String.equal (String.lowercase_ascii s) "njoin") then NJoin else
  if (String.equal (String.lowercase_ascii s) "nmeet") then NMeet else
  if (String.equal (String.lowercase_ascii s) "additive") then Additive else
  if (String.equal (String.lowercase_ascii s) "nadditive") then NAdditive else
  Any

let to_string (v : variance) : string = 
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