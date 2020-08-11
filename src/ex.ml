open Mu_calculus_syntax
open Variance_syntax

(* X antitone *)
let formula_simple : sugared_formula =
Mu("X", Ground, And( Diamond ("a", PreVariable("p")), (PreVariable("X"))))

let formula_untypable : sugared_formula =
	Mu("X", Ground, And( Diamond ("a", PreVariable("p")), Neg((PreVariable("X")))))

let formula_lambda : sugared_formula =
Mu("F", Arrow(Ground, Any, Ground), Lambda("X", Any, (And(PreVariable ("X"), Application((PreVariable ("F"), Diamond("a",PreVariable ("X"))))))))

let formula_two_lambdas : sugared_formula = 
Application(Mu("F", Arrow(Ground, Any, Ground), Lambda("Y", Any, ((Or(PreVariable ("Y"), Application((PreVariable ("F"),Diamond("b",PreVariable ("Y"))))))))), PreVariable("acc")) 