open Variance_syntax


type var = string

type mu_type =
  | Ground
  | Arrow of mu_type * variance * mu_type
  | Parameter of string

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
  
let rec t_to_string (tau : mu_type) : string =
  match tau with
    | Ground -> "Ground"
    | Arrow (tau1, v, tau2) -> t_to_string tau1 ^ " " ^ v_to_string v ^ " -> " ^ t_to_string tau2
    | Parameter (s) -> s


let rec f_to_string (phi : formula) : string =
  match phi with
    | Top -> "T"
    | Bottom -> "Bot"
    | Diamond (a, psi) -> " <" ^ a ^ "> "  ^ f_to_string psi
    | Box (a, psi) -> " [" ^ a ^ "] "  ^ f_to_string psi
    | And (psi, chi) -> f_to_string psi ^ " ^ " ^ f_to_string chi
    | Or (psi, chi) -> f_to_string psi ^ " U " ^ f_to_string chi
    | Neg (psi) -> "! " ^ f_to_string psi
    | PreVariable (x) -> x
    | Mu (f, tau, psi) -> "Mu"^ f ^":"^ t_to_string tau ^"."^ f_to_string psi
    | Nu (f, tau, psi) -> "Nu"^ f ^":"^ t_to_string tau ^"."^ f_to_string psi
    | Lambda (x, v, psi) -> "Lambda " ^ x ^ v_to_string v ^ " : Ground ." ^ f_to_string psi

let  ta_to_string (ta : type_assignment) : string =
  f_to_string ta.phi ^" ^ "^ v_to_string ta.variance ^" : "^ t_to_string ta.tau


let rec te_to_string (gamma : typing_environment) : string = 
  match gamma with 
    | [] -> ""
    | ta::g -> ta_to_string ta ^ "\n" ^ te_to_string g
