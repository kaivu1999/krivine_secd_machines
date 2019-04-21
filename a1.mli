
type exptype = Tint | Tunit | Tbool | Tfunc of (exptype * exptype)

(* abstract syntax *)
type expr = NIL
| V of string 
| Lambda of (expr * expr) 
| App of (expr * expr) 
(* unary operators on integers *)
| Abs of expr                   (* abs *)
| Negative of expr              (* unary minus ~ *)
| Cmp of expr 
(* binary operators on integers *)
| Integer of int 
| Plus of (expr * expr)
| Sub of (expr * expr)
| Mult of (expr * expr) 
| Div of (expr * expr)
| Rem of (expr * expr)
(* unary operators on Booleans *)
| Not of expr
(* binary operators on booleans *)
| Bool of bool 
| And of (expr * expr) 
| Or of (expr * expr) 
(* comparison operations on integers *)
| Equals of expr * expr      (* = *)
| GreaterTE of expr * expr   (* >= *)
| LessTE of expr * expr      (* <= *)
| GreaterT of expr * expr    (* > *)
| LessT of expr * expr       (* < *)
| If_Then_Else of (expr * expr * expr)
(* parenthesis *)
| Paren of expr

(* types for secd *)
and opcode = VAR of string 
          | NCONST of int 
          | BCONST of bool 
          | PLUS
          | MINUS
          | MULT
          | DIV
          | REM 
          | CMP 
          | ABS
          | NEG
          | EQ
          | GTE
          | LTE
          | GT
          | LT
          | NOT
          | AND 
          | ORR 
          | IFTE 
          | FABS 
          | FCALL 
          |  CLOS of string*(opcode list)
          | PAREN 
          | RET
and table_secd = string -> answer
and stack_secd = answer list
and dump = (stack_secd * table_secd * opcode list) list
and answer = Num of int 
          | BoolA of bool 
          | ValClosure of table_secd*string*(opcode list)

(* types for krivine *)
type closure = Clos of ( expr * ( string -> closure ) ) 
and value = Number of int 
          | Boolean of bool 
and table_krivine = string -> closure
and stack_krivine = closure list


val krivine : closure -> stack_krivine -> closure
val execute_krivine : expr -> table_krivine -> value


val secd : stack_secd ->  table_secd -> opcode list ->  dump -> answer
val execute_secd : expr -> table_secd -> answer
val compile : expr -> opcode list