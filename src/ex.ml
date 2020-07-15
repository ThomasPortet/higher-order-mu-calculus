open Mu_calculus_syntax
open Variance_syntax


(*
let formula_lambda : formula =
	Mu("F", Ground, Lambda([X;Y], [Any; Any], Union ( Inter (X, Y), F (Diamond ("a", X), Diamond ("b", Y)))))
*)
let formula_simple : formula =
	Mu("X", Ground, Union( Diamond ("a", Pre(PreVariable("p"))), Pre(PreVariable("X"))))