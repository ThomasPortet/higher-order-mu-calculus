open Variance_syntax


type var = string

type mu_type =
  | Ground
  | Arrow of mu_type * variance * mu_type
  | Parameter of string
  | Untypable

(*
  | Transformer of transformer

type transformer =
   predicate -> predicate
*)

type formula =
  | Top
  | Bottom
  | Diamond of var * (* * int for polyadic * *) formula
  | Box of var * (* * int for polyadic * *) formula
  | And of formula * formula
  | Or of formula * formula
  | Neg of formula
  | PreVariable of var
  | Mu of var * mu_type * formula (* smallest fix point *)
  | Nu of var * mu_type * formula (* greatest fix point *) (*
  | Application of transformer * formula list  *)
  | Lambda of var * variance  * formula      (*for higher order*)

type type_assignment = {
  phi : formula;
  variance : variance;
  tau : mu_type
  }

type typing_environment =
  type_assignment list

type type_judgment = {
  gamma : typing_environment;
  phi : formula;
  tau : mu_type
}


val te_to_string : typing_environment -> string