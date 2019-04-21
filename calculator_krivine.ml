open A1

exception Undefined_val

let rec rho_krivine s = match s with 
  "X" -> Clos (Integer (5), rho_krivine)
|  "TTrue" -> Clos (Bool (true), rho_krivine)
|  "fu" -> Clos (Lambda (V("x"),Plus(V("x"),Integer(1))), rho_krivine)
| "sa" -> Clos (App (Lambda (V "x", Paren (Plus (Integer 1, Integer 2))), Integer 3) , rho_krivine )
| _ -> raise Undefined_val


let print_value tr = match tr with
  Number a -> Pervasives.print_string (string_of_int a)
  | Boolean a -> Pervasives.print_string (string_of_bool a)
  | _ -> raise Undefined_val


let print_closure cl = match cl with 
  | Clos(Integer(i),env) -> Pervasives.print_string (string_of_int i)
  | Clos(Bool(b),env) -> Pervasives.print_string (string_of_bool b)
  | _ -> raise Undefined_val

(* This is the top level file for the expression evaluator. It is basically an infinite loop that consumes legal expression inputs
 * and prints their corresponding parse tree and evaluated output *)
let _ =
    Printf.printf "==> ";flush stdout;
    try
        let lexbuf = Lexing.from_channel stdin in
            while true do
            let result = A3.exp_parser A2.read lexbuf in
            (* Printf.printf "Answer: "; print_closure ( A1.krivine (Clos(result,rho_krivine)) [] ); Printf.printf "\n==> "; flush stdout *)
            Printf.printf "Answer: "; print_value ( A1.execute_krivine result rho_krivine ); Printf.printf "\n==> "; flush stdout
            (* flush ensures that the evaluated information gets printed to stdout *)
            done
        with A2.Eof ->
            exit 0