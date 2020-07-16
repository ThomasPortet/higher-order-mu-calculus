let usage = "mc.byte <file> [options]"

let in_channel = ref (stdin)

let compute_variance_simple = ref false

let composition = ref false

let smaller = ref false

let () = Arg.parse
    [
     "-compute_variance_simple", Set(compute_variance_simple), "compute the variance of the simple formula in ex.ml";
     "-smaller", Set(smaller), "compares two variances";
     "-composition", Set(composition), "composes two variances";
]

(fun filename -> in_channel := open_in filename)
    usage
