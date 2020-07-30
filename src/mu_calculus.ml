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

let rec get_assignment (gamma : typing_environment) (phi : formula) : type_assignment option = 
  match gamma with
    | [] -> None
    | t::g when (t.phi == phi) -> Some(t)
    | _::g -> get_assignment g phi

let rec get_variable_assignment (gamma : typing_environment) (x : var) : type_assignment option = 
  match gamma with
    | [] -> None
    | t::g -> (match t.phi with  
                  | PreVariable(y) -> if (String.equal y x) then  Some(t)
                else get_variable_assignment g x
                  | _ -> get_variable_assignment g x)


(* returns gamma' = v o gamma *)
let composition_environment (v : variance) (gamma : typing_environment) : typing_environment =
  List.map (fun  (ta : type_assignment) -> {phi = ta.phi; variance = (composition v ta.variance); tau = ta.tau})  gamma

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
        | ta::te ->  (inverse_assignment v ta) :: (filter_gamma te) 
  in
  let rec filter_assignments (inv_list : type_assignment list list) : typing_environment list =

      match inv_list with 
        | [] ->  [[]]
        | tal::invl ->   match tal with 
                          | [] -> (filter_assignments invl)
                          | ta::l ->  (cons_list_list ta (filter_assignments invl))@(filter_assignments (l::invl)) 
    in filter_assignments (filter_gamma gamma)

(* returns a list containing all the bigger assignments than ta *)
let bigger_assignment  (ta : type_assignment) : type_assignment list = 
  let rec filter_variance (vl : variance list) : type_assignment list =
      match vl with
        | [] -> []
        | v'::l -> {phi = ta.phi; variance = v'; tau = ta.tau}:: filter_variance l
    in filter_variance (bigger_variances ta.variance)

(* returns a list containing the environments bigger or equal to gamma *)
let  bigger_environments (gamma : typing_environment) : typing_environment list = 
  let rec filter_gamma (gamma : typing_environment) : type_assignment list list =
      match gamma with
        | [] -> []
        | ta::te -> bigger_assignment ta :: (filter_gamma te) 
  in
  let rec filter_assignments (inv_list : type_assignment list list) : typing_environment list =
      match inv_list with 
        | [] -> []
        | tal::invl -> match tal with 
                          | [] -> []
                          | ta::l -> (cons_list_list ta (filter_assignments invl))@(filter_assignments (l::invl))
    in filter_assignments (filter_gamma gamma)

(* returns the negated typing environment of gamma *)
let rec not_e (gamma : typing_environment) : typing_environment =
  match gamma with 
    | [] -> []
    | ta::g -> {phi = ta.phi ; variance = not_v ta.variance; tau = ta.tau}::not_e g

let rec type_inference (gamma : typing_environment) (input_formula : formula) : mu_type =
 (* let () = print_string ("phi : " ^(f_to_string input_formula)^"\ngamma : "^(te_to_string gamma)) in *) 
  let rec filter_gammas (gammas : typing_environment list ) (phi : formula) : mu_type =
    match gammas with
      | [] ->  Untypable
      | gamma1 :: gamma_list -> let tau = type_inference gamma1 phi in 
                               match tau with 
                                  | Untypable ->  filter_gammas gamma_list phi
                                  | _ ->  tau (* we only care about finding any type for now *)
    in
    let rec filter_appl_pairs (pairs : (typing_environment * typing_environment) list) (f : formula) (phi : formula) : mu_type = 
              match pairs with
                 | [] -> Untypable
                 | (gamma1,vgamma2) :: g -> let arrt = type_inference gamma1 f in 
                                                match arrt with 
                                                | Arrow (Ground, v, tau) -> let rec filter_gamma_twos (gammas : typing_environment list) : mu_type =
                                                                                match gammas with 
                                                                                    | [] -> filter_appl_pairs g f phi
                                                                                    | gamma2::gl -> match (type_inference gamma2 phi) with
                                                                                                       | Ground -> tau
                                                                                                       | _ -> filter_gamma_twos gl
                                                                            in
                                                                            let gammatwos = inverse_environment v vgamma2 in
                                                                            filter_gamma_twos gammatwos

                                                | _-> filter_appl_pairs g f phi
    in
    match input_formula with
      | Top -> Ground
      | Neg (phi) -> type_inference (not_e gamma) phi 
      | Diamond (a, phi) -> filter_gammas (inverse_environment Meet gamma) phi 
      | And (phi, psi) -> let tau1,tau2 = type_inference gamma phi, type_inference gamma psi in (match (tau1,tau2)with 
                            | Ground, Ground -> Ground  
                            | _,_ ->  failwith "And" ) (* there isn't a product type so there can't be a t1 * t2 type for instance, the only way to type the 'and' is if both have a ground type *)
      | PreVariable (x) -> (match (get_variable_assignment gamma x) with
                      | Some (ta)  -> ta.tau
                      | _ -> failwith "Var") (* there shouldn't be any free variable at this point *)
      | Mu (f, t, phi) -> (match (t, get_variable_assignment gamma f) with
                              | _, Some(ta) ->  failwith "Mu"(* f should be a new transformer or a new predicate variable *)
                              | Ground, _ -> let new_gamma = replace_type_assignment gamma {phi = PreVariable(f); variance = Monotone; tau = t} in type_inference new_gamma phi
                              | _, _ -> let new_gamma = replace_type_assignment gamma {phi = PreVariable(f); variance = Monotone; tau = t} in type_inference new_gamma phi )
      | Lambda (x, v, phi) -> (match (get_variable_assignment gamma x) with
                      | None -> let new_gamma = replace_type_assignment gamma {phi = PreVariable(x); variance = v; tau = Ground} in
                                let tau = type_inference new_gamma phi in Arrow (Ground, v, tau)
                      | _ -> Untypable ) (* x is supposed to be a new variable *)
      | Application (f ,phi) -> 
        let gammas = bigger_environments gamma in 
          let gamma_pairs = permutation_pairs gammas gammas in (*contains all the pairs gamma1, vgamma2 st gamma <= gamma1 and gamma <= vgamma2 *)
              filter_appl_pairs gamma_pairs f phi
                                            

let print_infered_type (f : formula) : unit =
  let tau = type_inference [{phi =  PreVariable ("p"); variance = Any; tau = Ground}] f in 
  print_string ((t_to_string tau)^"\n")