open Mu_calculus_syntax
open Variance_syntax

let formula_simple : formula =
	Mu("X", Ground, Or( Diamond ("a", PreVariable("p")), Neg(PreVariable("X"))))

let formula_untypable : formula =
	Mu("X", Ground, Nu("X", Ground, Or( Diamond ("a", PreVariable("p")), PreVariable("X"))))

let formula_lambda : formula =
Mu("F", Ground, Lambda("X", Any, Lambda("Y", Any, Or(Mu("X", Ground, PreVariable("X")) , Mu("Y", Ground, PreVariable("Y"))))))