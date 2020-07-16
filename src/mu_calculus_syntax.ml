open Variance_syntax

type var = string

type mu_type = 
  | Ground
  | Arrow of mu_type * variance * mu_type

type predicate = 
  | PreVariable of var(*     for higher order
  | Transformer of transformer

type transformer =
   predicate -> predicate
*)

type formula =
  | True
  | Bottom
  | Diamond of var * (* * int for polyadic * *) formula
  | Box of var * (* * int for polyadic * *) formula
  | Inter of formula * formula
  | Union of formula * formula 
  | Neg of formula
  | Pre of predicate
  | Mu of var * mu_type * formula (* smallest fix point *)
  | Nu of var * mu_type * formula (* greatest fix point *) (*
  | Application of transformer * formula list  
  | Lambda of var list * variance list * formula      for higher order*)
