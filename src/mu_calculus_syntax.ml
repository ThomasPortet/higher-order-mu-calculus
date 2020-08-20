open Variance_syntax


type var = string

type mu_type =
  | Ground
  | Arrow of mu_type * variance * mu_type
  | Parameter of string
  | Untypable

type sugared_formula =
  | Top
  | Bottom
  | And of sugared_formula * sugared_formula
  | Or of sugared_formula * sugared_formula
  | Neg of sugared_formula
  | Diamond of var * (* * int for polyadic * *) sugared_formula
  | Box of var * (* * int for polyadic * *) sugared_formula
  | PreVariable of var
  | Mu of var * mu_type * sugared_formula (* smallest fix point *)
  | Nu of var * mu_type * sugared_formula (* greatest fix point *) 
  | Lambda of var * sugared_formula      (*for higher order*) 
  | Application of sugared_formula * sugared_formula


type formula =
  | Top
  | And of formula * formula
  | Neg of formula
  | Diamond of var * (* * int for polyadic * *) formula
  | PreVariable of var
  | Mu of var * mu_type * formula (* smallest fix point *)
  | Lambda of var  * formula      (*for higher order*) 
  | Application of formula * formula

let rec neg_var (phi : formula) (x : var) : formula =
  match phi with 
    | Top -> Top
    | And (phi, psi) -> And (neg_var phi x, neg_var psi x)
    | Neg (phi) -> Neg(neg_var phi x)
    | Diamond (a, phi) -> Diamond (a, neg_var phi x)
    | PreVariable (y) ->  if (String.equal x y) then Neg(phi) else phi
    | Mu (y,tau,phi) -> Mu(y,tau,neg_var phi x)
    | Lambda (y, phi) -> Lambda (y, neg_var phi x)
    | Application (phi,psi) -> Application(neg_var phi x, psi)

let rec desugar (sf : sugared_formula) : formula =
  match sf with 
  | Top -> Top
  | Bottom -> Neg Top
  | And (phi, psi) -> And (desugar phi, desugar psi)
  | Or (phi,psi) -> Neg (And (Neg(desugar phi), Neg(desugar psi)))
  | Neg (phi) -> Neg(desugar phi)
  | Diamond (a,phi) -> Diamond (a, (desugar phi))
  | Box (a,phi) -> Neg (Diamond (a, Neg(desugar phi)))
  | PreVariable (x) -> PreVariable(x)
  | Mu (x,t,phi) -> Mu (x, t, desugar phi)
  | Nu (x,t,phi) -> Neg (Mu (x, t, Neg( neg_var (desugar phi) x)))
  | Lambda (x,phi) ->  Lambda (x, desugar phi)
  | Application (phi,psi) -> Application (desugar phi, desugar psi)
    


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
    | Untypable -> "Untypable"


let rec sf_to_string (phi : sugared_formula) : string =
  match phi with
    | Top -> "T"
    | Bottom -> "Bot"
    | Diamond (a, psi) -> " <" ^ a ^ "> "  ^ sf_to_string psi
    | Box (a, psi) -> " [" ^ a ^ "] "  ^ sf_to_string psi
    | And (psi, chi) -> sf_to_string psi ^ " ^ " ^ sf_to_string chi
    | Or (psi, chi) -> sf_to_string psi ^ " U " ^ sf_to_string chi
    | Neg (psi) -> "! " ^ sf_to_string psi
    | PreVariable (x) -> x
    | Mu (f, tau, psi) -> "Mu "^ f ^":"^ t_to_string tau ^".("^ sf_to_string psi^")"
    | Nu (f, tau, psi) -> "Nu "^ f ^":"^ t_to_string tau ^"."^ sf_to_string psi
    | Lambda (x, psi) -> "Lambda " ^ x  ^ " : Ground ." ^ sf_to_string psi
    | Application (f, psi) -> sf_to_string f ^ sf_to_string psi

let rec f_to_string (phi : formula) : string =
  match phi with
    | Top -> "T"
    | Diamond (a, psi) -> "<" ^ a ^ "> "  ^ f_to_string psi^""
    | And (psi, chi) -> f_to_string psi ^ " ^ " ^ f_to_string chi
    | Neg (psi) -> "!(" ^ f_to_string psi^")"
    | PreVariable (x) -> x
    | Mu (f, tau, psi) -> "Mu "^ f ^":"^ t_to_string tau ^".("^ f_to_string psi^")"
    | Lambda (x, psi) -> "Lambda " ^ x ^ " : Ground ." ^ f_to_string psi
    | Application (f, psi) -> f_to_string f ^ f_to_string psi


let  ta_to_string (ta : type_assignment) : string =
  f_to_string ta.phi ^" ^ "^ v_to_string ta.variance ^" : "^ t_to_string ta.tau


let rec te_to_string (gamma : typing_environment) : string = 
  match gamma with 
    | [] -> ""
    | ta::g -> ta_to_string ta ^ "\n" ^ te_to_string g
