let usage = "mc.byte <file> [options]"

let in_channel = ref (stdin)

let composition = ref false

let smaller = ref false

let compute_variance_simple = ref false

let compute_variance_untypable = ref false

let compute_variance_lambda = ref false


let () = Arg.parse
    [
     "-composition", Set(composition), "composes two variances";
     "-smaller", Set(smaller), "compares two variances";
     "-compute_variance_simple", Set(compute_variance_simple), "compute the variance of the simple formula in ex.ml";
     "-compute_variance_untypable", Set(compute_variance_untypable), "compute the variance of the untypable formula in ex.ml";
     "-compute_variance_lambda", Set(compute_variance_lambda), "compute the variance of the lambda formula in ex.ml";
 
     
]

(fun filename -> in_channel := open_in filename)
    usage
