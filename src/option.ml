let usage = "mc.byte <file> [options]"

let in_channel = ref (stdin)

let compute_variance = ref false

let smaller = ref false

let () = Arg.parse
    [
     "-compute_variance", Set(compute_variance), "compute the variance of the input formula";
     "-smaller", Set(smaller), "compares two variances";
  
  
]

(fun filename -> in_channel := open_in filename)
    usage
