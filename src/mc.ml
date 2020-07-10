open Variance_syntax
open Frontend
open Mu_calculus_syntax
open Variance

(*
let input_formula =
  Frontend.parse_lexbuf Format.std_formatter ( Lexing.from_channel Option.(!in_channel) )
*
let formula = mu_calculus_syntax.from_file input_formula

let () = if Option.(!compute_variance) then begin
    Variance.print_computed formula
  end*)
let () = if Option.(!smaller) then begin
  let () = print_string "première variance: " in
  let s1 = read_line () in
  let () = print_string "deuxième variance: " in
  let s2 = read_line () in
  let v1 = Variance_syntax.variance_from_string s1 in 
  let v2 = Variance_syntax.variance_from_string s2 in

  if (Variance.smaller v1 v2) then (print_string "true\n") else (print_string "false\n")
end