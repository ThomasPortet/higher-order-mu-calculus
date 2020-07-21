open Mu_calculus_syntax
open Variance_syntax
open Variance

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

let composition_assignment (v : variance) (ta : type_assignment) : type_assignment =  
  {phi = ta.phi; variance = (composition v ta.variance); tau = ta.tau}

let composition_environment (v : variance) (gamma : typing_environment) : typing_environment =
  List.map (composition_assignment v) gamma

let rec not_e (gamma : typing_environment) : typing_environment =
  match gamma with 
    | [] -> []
    | ta::g -> {phi = ta.phi ; variance = not_v ta.variance; tau = ta.tau}::not_e g 


let type_inference (input_formula : formula) : type_judgment =
  let rec aux (form : formula) (tj : type_judgment) : type_judgment = 
    match form with
      | Top -> tj
      | Bottom -> {gamma = not_e tj.gamma ; phi = tj.phi ; tau = tj.tau} (*
      | Diamond (a, f) -> add_constraint smaller composition_environment Meet gamma 
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
      | _ -> tj

  in aux input_formula {gamma = []; phi = input_formula; tau= Parameter("tau")}