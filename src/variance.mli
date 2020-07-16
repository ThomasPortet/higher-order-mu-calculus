open Variance_syntax
open Mu_calculus_syntax

val smaller : variance -> variance -> bool

val dual : variance -> variance

val not : variance -> variance

val inter : variance -> variance -> variance

val composition : variance -> variance -> variance

val composition_meet : variance -> variance

val variances_needed : formula -> variance_assignment list option 

val assignment  : var -> variance_assignment list -> variance option 

val assign_variance : variance_assignment -> variance_assignment list -> variance_assignment list option 

val negated_variances : variance_assignment list -> variance_assignment list