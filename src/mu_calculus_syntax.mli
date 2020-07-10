open Variance_syntax
(*
type var = string

type mu_type = 
  | Ground
  | Arrow of mu_type * variance * mu_type

type predicate = 
  | PreVariable
  | Transformer of transformer

type transformer = 
   predicate -> predicate


type formula =
  | True
  | Diamond of var * formula
  | Inter of formula * formula
  | Neg of formula
  | Pre of predicate
  | Mu of predicate * mu_type * formula
  | Application of transformer * formula
  | Lambda of predicate * variance * formula
*)