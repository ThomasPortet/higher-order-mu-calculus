let usage = "mc.byte <file> [options]"

let in_channel = ref (stdin)

let compute_variance = ref false

let () = Arg.parse
    [
     "-compute_variance", Set(compute_variance), "compute the variance of the input formula";
  
]

(fun filename -> in_channel := open_in filename)
    usage
