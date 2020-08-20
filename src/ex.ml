open Mu_calculus_syntax
open Variance_syntax

(* X antitone *)
let formula_simple : sugared_formula =
Mu("X", Ground, And( Diamond ("a", PreVariable("p")), (PreVariable("X"))))

let formula_untypable : sugared_formula =
	Mu("X", Ground, And( Diamond ("a", PreVariable("p")), Neg((PreVariable("X")))))

let formula_lambda : sugared_formula =
Mu("F", Arrow(Ground, Any, Ground), Lambda("X",  (And(PreVariable ("X"), Application((PreVariable ("F"), Diamond("a",PreVariable ("X"))))))))

let formula_two_lambdas : sugared_formula = 
Application(Nu("E", Arrow(Ground, Any, Ground), Lambda("X",  (And(PreVariable("X"), Application(PreVariable("E"), Diamond("a",PreVariable("X"))))))) , Application(Mu("F", Arrow(Ground, Any, Ground), Lambda("Y", ((Or(PreVariable ("Y"), Application((PreVariable ("F"),Diamond("b",PreVariable ("Y"))))))))), PreVariable("acc"))) 

let formula_paper : sugared_formula = 
Application (Mu("F", Arrow(Ground,Any,Ground), Lambda("X", Diamond("a", And(PreVariable("Y"), Application(PreVariable("F"), Neg(Application(PreVariable("F"), PreVariable("X")))))))), Box("b", PreVariable("Y")))