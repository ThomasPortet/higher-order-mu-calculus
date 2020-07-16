{
open Variance_syn
open Mu_yacc


let keywords = Hashtbl.create 30
let _ =
  Array.iter
    (fun (keyword,token) -> Hashtbl.add keywords keyword token)
    [|
      ("formula", TK_FORMULA);
      ("mu", TK_MU);
      ("lambda", TK_LAMBDA);
      ("variable", TK_VARIABLE);
      ("none", TK_NONE;
      ("any", TK_ANY);
      ("inter", TK_INTER);
      ("union", TK_UNION);
      ("variance", TK_VARIANCE);
      ("tau", TK_TAU);
      
    |]