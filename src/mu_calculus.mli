open Mu_calculus_syntax
open Variance_syntax

val smaller_environment  : typing_environment -> typing_environment -> bool 

val composition_environment : variance -> typing_environment -> typing_environment

val print_infered_type : formula -> unit