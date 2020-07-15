open Variance_syntax
(*
type var = string

type mu_type = 
  | Ground
  | Arrow of mu_type * variance * mu_type

type predicate = 
  | PreVariable
  | Transformer

type formula =
  | True
  | False
  | Diamond of string * formula
  | Inter of formula * formula
  | Neg of formula
  | Pre of predicate
  | Mu of variable * mu_type * formula
  | Application of transformer * formula
  | Lambda of variable * variance * formula
*)