#directory "_build";; (* Consider this folder when looking for files *)
#load "a1.cmo";; (* Load the a0 bytecode *)
#load "a2.cmo";;
#load "a3.cmo";;
open A1;;
open A2;;
open A3;;

exception Not_implemented
(* Helper function to print *)
(* let rec print_tree tr = match tr with
  Integer a -> "INT " ^ (string_of_int a)
  | _ -> raise Not_implemented
;;
let rec print_answer tr = match tr with
  Num a -> print_num a
  | BoolA a -> string_of_bool a
  | ValClosure(tb,str,opList) -> print_string str
  | _ -> raise Not_implemented
;;

let rec print_value tr = match tr with
  Number a -> string_of_int a
  | Boolean a -> string_of_bool a
  | _ -> raise Not_implemented
;;

(* Input is given as value and output is an answer *)
let rec toAnswer v = match v with
  Number a     -> Num  a
| Boolean b    -> BoolA b
| ValClosure vl -> raise Not_implemented
(* Input is given as string and output is an answer *)
let binding rho s = toAnswer (rho s);; *)

(* Parser accepts the expression as string and binding as hash map with variable to values (integer, boolean, tuple) *)
let parser (s:string) rho =
  let result = A3.exp_parser A2.read (Lexing.from_string s) in
    (* Return the three versions as abstract syntax tree, value, compiled opcode*)
    result
     (* (A1.execute_krivine result rho), (A1.execute_secd result (binding rho) (A1.compile result))) *)
;;

(* Input is given as string and output is a value *)
let rec rho_krivine s = match s with 
  "X" -> Clos (Integer (5), rho_krivine)
|  "TTrue" -> Clos (Bool (true), rho_krivine)
| _ -> raise Not_implemented
;;



let rec rho_secd s = match s with 
  "X" -> Num(5)
| "fuc" -> ValClosure (rho_secd,"x",[CLOS ("x", [VAR "x"; NCONST 1; A1.MINUS; RET])])])
| "TTrue" -> BoolA(true)
| _ -> raise Not_implemented
;;

let _ = (parser "TTrue" rho);;
let _ = (parser "10 + ~5" rho);;
let _ = (parser "if 5 > 6 then 5 else 6 fi" rho);;
let _ = (parser "10 + ~5" rho);;
let _ = (parser "TTrue" rho);;
let _ = (parser "2 - 3 + 4" rho);;
let _ = (parser "(1 + 2)" rho);;
let s = "(2 + 3)" ;;
let result = A3.main A2.read (Lexing.from_string s) ;;
result ;;
A1.compile result ;;
A1.eval result rho ;;
let ans = A1.stackmc [] (binding rho) (A1.compile result);;