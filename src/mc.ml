open Variance_syntax
open Mu_calculus_syntax
open Variance
open Mu_calculus
open Ex

(*
let input_formula =
  Frontend.parse_lexbuf Format.std_formatter ( Lexing.from_channel Option.(!in_channel) )
*
let formula = mu_calculus_syntax.from_file input_formula

let () = if Option.(!compute_variance) then begin
    Variance.print_computed formula
  end*)
let () = if Option.(!smaller) then 
  let () = print_string "première variance: " in
  let s1 = read_line () in
  let () = print_string "deuxième variance: " in
  let s2 = read_line () in
  let v1 = Variance_syntax.variance_from_string s1 in 
  let v2 = Variance_syntax.variance_from_string s2 in
  if (Variance.smaller v1 v2) then (print_string "true\n") else (print_string "false\n")


let () = if Option.(!composition) then 
  let () = print_string "première variance: " in
  let s1 = read_line () in
  let () = print_string "deuxième variance: " in
  let s2 = read_line () in
  let v1 = Variance_syntax.variance_from_string s1 in 
  let v2 = Variance_syntax.variance_from_string s2 in
  let v = Variance.composition v1 v2 in
  let s = v_to_string v in 
  print_string (s^"\n")

let () = if Option.(!compute_variance_simple) then 
  let f = desugar formula_simple in 
  begin 
  print_string ((f_to_string f)^"\n") ;
  print_compute_variance f
  end

let () = if Option.(!compute_variance_untypable) then
let f = desugar formula_untypable in 
  begin 
  print_string ((f_to_string f)^"\n") ;
  print_compute_variance f
  end 

let () = if Option.(!compute_variance_lambda) then 
let f = desugar formula_lambda in 
  begin 
  print_string ((f_to_string f)^"\n") ;
  print_compute_variance f
  end 

let () = if Option.(!type_inference_simple) then 
 let f = desugar formula_simple in 
  begin 
  print_string ((f_to_string f)^"\n") ;
  print_infered_type f
  end

let () = if Option.(!type_inference_untypable) then 
  let f = desugar formula_untypable in 
  begin 
  print_string ((f_to_string f)^"\n") ;
  print_infered_type f
  end

let () = if Option.(!type_inference_lambda) then 
  let f = desugar formula_lambda in 
  begin 
  print_string ((f_to_string f)^"\n") ;
  print_infered_type f
  end

let () = if Option.(!type_inference_two_lambdas) then 
  let f = desugar formula_two_lambdas in 
  begin 
  print_string ((f_to_string f)^"\n") ;
  print_infered_type f
  end

let () = if Option.(!type_inference_paper) then 
  let f = desugar formula_paper in 
  begin 
  print_string ((f_to_string f)^"\n") ;
  print_infered_type f
  end