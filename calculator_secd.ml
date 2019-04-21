open A1

exception Undefined_val
exception Var_NotDefined

let rec rho_secd s = match s with 
  "X" -> Num(5)
| "TTrue" -> BoolA(true)
| "f" -> ValClosure (rho_secd,"x",[NCONST(1);VAR("x");PLUS])
| "fu" -> ValClosure (rho_secd,"x",[CLOS("x",[VAR("fu");VAR("x");NCONST(1);MINUS;FCALL;NCONST(1);PLUS;RET])])
| "fuc" -> ValClosure (rho_secd,"x",[CLOS("x",[VAR("x");NCONST(1);MINUS;RET])])
| _ -> raise Var_NotDefined

let print_value tr = match tr with
  Num a -> Pervasives.print_string (string_of_int a)
  | BoolA a -> Pervasives.print_string (string_of_bool a)
  | _ -> Pervasives.print_string "Is answer a ValClosure ? Answers other than Num(a) and BoolA(b) are not supported :) "

(* This is the top level file for the expression evaluator. It is basically an infinite loop that consumes legal expression inputs
 * and prints their corresponding parse tree and evaluated output *)
let _ =
    Printf.printf "==> ";flush stdout;
    try
        let lexbuf = Lexing.from_channel stdin in
            while true do
            let result = A3.exp_parser A2.read lexbuf in
            Printf.printf "Answer: "; print_value ( A1.execute_secd result rho_secd ); Printf.printf "\n==> "; flush stdout
            (* flush ensures that the evaluated information gets printed to stdout *)
            done
        with A2.Eof ->
            exit 0