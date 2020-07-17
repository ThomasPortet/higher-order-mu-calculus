open Mu_calculus_syntax
open Variance_syntax

let formula_simple : formula =
	Mu("X", Ground, Or( Diamond ("a", Pre(PreVariable("p"))), Neg(Pre(PreVariable("X")))))

let formula_untypable : formula =
	Mu("X", Ground, Nu("X", Ground, Or( Diamond ("a", Pre(PreVariable("p"))), Pre(PreVariable("X")))))

let formula_lambda : formula =
Mu("F", Ground, Lambda("X", Any, Lambda("Y", Any, Or(Mu("X", Ground, Pre(PreVariable("X"))) , Mu("Y", Ground, Pre(PreVariable("Y")))))))