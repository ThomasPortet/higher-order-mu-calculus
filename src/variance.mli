open Variance_syntax
open Mu_calculus_syntax

val smaller : variance -> variance -> bool
val dual : variance -> variance
val not : variance -> variance
val inter : variance -> variance -> variance
val composition : variance -> variance -> variance
val composition_meet : variance -> variance
(* val compute_variance : formula -> variance*)