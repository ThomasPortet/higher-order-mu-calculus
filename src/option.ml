let usage = "mc.byte <file> [options]"

let in_channel = ref (stdin)

let composition = ref false

let smaller = ref false

let compute_variance_simple = ref false

let compute_variance_untypable = ref false

let compute_variance_lambda = ref false

let type_inference_simple = ref false

let type_inference_untypable = ref false

let type_inference_lambda = ref false

let type_inference_two_lambdas = ref false

let type_inference_paper = ref false

let () = Arg.parse
    [
     "-composition", Set(composition), "composes two variances";
     "-smaller", Set(smaller), "compares two variances";
     "-compute_variance_simple", Set(compute_variance_simple), "compute the variance of the simple formula in ex.ml";
     "-compute_variance_untypable", Set(compute_variance_untypable), "compute the variance of the untypable formula in ex.ml";
     "-compute_variance_lambda", Set(compute_variance_lambda), "compute the variance of the lambda formula in ex.ml";
     "-type_inference_simple", Set(type_inference_simple), "compute the type of the simple formula in ex.ml";
     "-type_inference_untypable", Set(type_inference_untypable), "compute the type of the untypable formula in ex.ml";
     "-type_inference_lambda", Set(type_inference_lambda), "compute the type of the lambda formula in ex.ml";
     "-type_inference_two_lambdas", Set(type_inference_two_lambdas), "compute the type of the two-lambdas formula in ex.ml";
     "-type_inference_paper", Set(type_inference_paper), "compute the type of the formula given as exameple in the paper in ex.ml";
]

(fun filename -> in_channel := open_in filename)
    usage
