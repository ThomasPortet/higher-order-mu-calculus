open Mu_calculus_syntax
open Variance_syntax
open Variance
open Util

(* a typing environment is smaller or equal than another if they differ only with respects to the variances,
  and all variances from the first typing environment are smaller or equal than the ones for the same variable in the second one *)
let rec smaller_environment (gamma1 : typing_environment) (gamma2 : typing_environment) : bool =
  let rec fetch_bigger (ta : type_assignment) (gamma : typing_environment) : typing_environment option = 
    match gamma with 
      | [] -> None
      | ta2::g2 -> if (ta.phi == ta2.phi) then 
                        (if not((smaller ta.variance  ta2.variance) && (ta.tau == ta2.tau)) then None else Some (g2)) (* leaving out ta2 *)
                   else match (fetch_bigger ta g2) with
                        | Some (g) -> Some(ta::g)
                        | None -> None
  in
  match (gamma1, gamma2) with
    | [], [] -> true
    | [], _ | _, [] -> false
    | ta::g1, _ -> match fetch_bigger ta gamma2 with
                    | Some(g2) -> smaller_environment g1 g2
                    | None -> false

(* adds an assignment to a typing environment and eventually deletes a previous assignment of the same formula *)
let rec replace_type_assignment  (gamma : typing_environment) (ta : type_assignment) : typing_environment = 
  match gamma with
    | [] -> [ta]
    | t::g when (t.phi == ta.phi) -> ta::g 
    | t::g -> t::(replace_type_assignment g ta)

let composition_assignment (v : variance) (ta : type_assignment) : type_assignment =  
  {phi = ta.phi; variance = (composition v ta.variance); tau = ta.tau}

(* returns gamma' = v o gamma *)
let composition_environment (v : variance) (gamma : typing_environment) : typing_environment =
  List.map (composition_assignment v) gamma

let inverse_assignment (v : variance) (ta : type_assignment) : type_assignment list = 
  let rec filter_variance (vl : variance list) : type_assignment list =
      match vl with
        | [] -> []
        | v'::l -> {phi = ta.phi; variance = v'; tau = ta.tau}:: filter_variance l
    in filter_variance (inverse_variance_composition v ta.variance)


(* gives all the possible combinations of typing environments st that environment is the inverse of gamma in respect to v *)
let inverse_environment (v : variance) (gamma : typing_environment) : typing_environment list = 
  let rec filter_gamma (gamma : typing_environment) : type_assignment list list =
      match gamma with
        | [] -> []
        | ta::te -> inverse_assignment v ta :: (filter_gamma te) 
  in
  let rec filter_assignments (inv_list : type_assignment list list) : typing_environment list =
      match inv_list with 
        | [] -> []
        | tal::invl -> match tal with 
                          | [] -> []
                        | ta::tala -> (cons_list_list ta (filter_assignments invl))@(filter_assignments (tala::invl))

    in filter_assignments (filter_gamma gamma)




(* returns the negated typing environment of gamma *)
let rec not_e (gamma : typing_environment) : typing_environment =
  match gamma with 
    | [] -> []
    | ta::g -> {phi = ta.phi ; variance = not_v ta.variance; tau = ta.tau}::not_e g 


let rec type_inference (gamma : typing_environment) (phi : formula) : mu_type =
  let rec filter_gammas (gammas : typing_environment list ) (phi : formula) : mu_type =
    match gammas with
      | [] -> failwith " untypable "
      | gamma :: gamma_list -> type_inference gamma phi (*TODO UNION*)
    in
    match phi with
      | Top -> Ground
      | Bottom -> type_inference (not_e gamma) phi 
      | Diamond (a, psi) -> filter_gammas (inverse_environment Meet gamma) phi (*
      | Box of var *formula
      | And of formula * formula
      | Or of formula * formula
      | Neg of formula
      | Pre of predicate
      | Mu of var * mu_type * formula 
      | Nu of var * mu_type * formula 
      | Application of transformer * formula list  
      
    
      | Lambda (x, v, psi) -> match {phi = x; variance = v; phi = psi}::tj.gamma
    *)
      | _ -> Ground
(*
let type_inference (input_formula : formula) : type_judgment =
  let rec aux (form : formula) (tj : type_judgment) : type_judgment = 
    match form with
      | Top -> tj (* proved *)
      | Bottom -> {gamma = not_e tj.gamma ; phi = tj.phi ; tau = tj.tau} 
      | Diamond (a, f) ->  smaller composition_environment Meet gamma 
      | Box of var *formula
      | And of formula * formula
      | Or of formula * formula
      | Neg of formula
      | Pre of predicate
      | Mu of var * mu_type * formula 
      | Nu of var * mu_type * formula 
      | Application of transformer * formula list  
      
    
      | Lambda (x, v, psi) -> match {phi = x; variance = v; phi = psi}::tj.gamma
    
      | _ -> tj

  in aux input_formula {gamma = []; phi = input_formula; tau= Parameter("tau")}*)