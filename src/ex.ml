open Mu_calculus_syntax
open Variance_syntax

(* X antitone *)
let formula_simple : sugared_formula =
Mu("X", Ground, And( Diamond ("a", PreVariable("p")), (PreVariable("X"))))

let formula_untypable : sugared_formula =
	Mu("Y", Ground, Nu("X", Arrow (Ground, Any, Ground), And( Diamond ("a", PreVariable("p")), PreVariable("X"))))

let formula_lambda : sugared_formula =
Mu("F", Ground, Lambda("X", Any, Lambda("Y", Any, (Or(And(PreVariable ("X"), PreVariable("Y")) , Application(Application(PreVariable ("F"), Diamond("a",PreVariable ("X"))), Diamond("b",PreVariable("Y"))))))))