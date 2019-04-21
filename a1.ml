exception Not_implemented
exception Closure_Error
exception Type_Error


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


let rec update_table_krivine (str:string) (cl:closure) (env:table_krivine) :table_krivine = (let func (stra:string) = (match stra with
                                                                               str -> cl
                                                                              | _ -> (env str) )  in 
                                                              func)

                                                              
(* (Clos(Integer(i1),envd) , Clos(Plus(NIL,exp2),env)::s) -> match (krivine (Clos(exp2,env)) s) with
                                                        Clos(Integer(i),envdd) -> krivine (Clos(Integer(i1 + i2),env)) s
|  (Clos(Integer(i1),envd) , Clos(Mult(NIL,exp2),env)::s) -> match (krivine (Clos(exp2,env)) s) with
                                                        Clos(Integer(i),envdd) -> krivine (Clos(Integer(i1 * i2),env)) s *)

let rec krivine cl stack = match cl with
  Clos(Integer(i),env)  -> cl
| Clos(V(str),env) -> krivine (env str) stack
| Clos(Plus(exp1,exp2),env) -> (match ((krivine (Clos(exp1,env)) stack) , (krivine (Clos(exp2,env)) stack)) with 
                            (Clos(Integer(i1),envd1),Clos(Integer(i2),envd2)) -> Clos(Integer(i1+i2),env)
                            | _ -> raise Type_Error)
| Clos(Sub(exp1,exp2),env) ->( match ((krivine (Clos(exp1,env)) stack) , (krivine (Clos(exp2,env)) stack)) with 
                            (Clos(Integer(i1),envd1),Clos(Integer(i2),envd2)) -> Clos(Integer(i1 - i2),env)
                            | _ -> raise Type_Error)
| Clos(Mult(exp1,exp2),env) ->( match ((krivine (Clos(exp1,env)) stack) , (krivine (Clos(exp2,env)) stack)) with 
                            (Clos(Integer(i1),envd1),Clos(Integer(i2),envd2)) -> Clos(Integer(i1*i2),env)
                            | _ -> raise Type_Error)
| Clos(Div(exp1,exp2),env) ->( match ((krivine (Clos(exp1,env)) stack) , (krivine (Clos(exp2,env)) stack)) with 
                            (Clos(Integer(i1),envd1),Clos(Integer(i2),envd2)) -> Clos(Integer(i1/i2),env)
                            | _ -> raise Type_Error)
| Clos(Rem(exp1,exp2),env) ->( match ((krivine (Clos(exp1,env)) stack) , (krivine (Clos(exp2,env)) stack)) with 
                            (Clos(Integer(i1),envd1),Clos(Integer(i2),envd2)) -> Clos(Integer(i1 mod i2),env)
                            | _ -> raise Type_Error)
| Clos(Cmp(exp1),env) -> (match (krivine (Clos(exp1,env)) stack) with 
                          (Clos(Integer(i),env1)) -> if (i=0) then (Clos(Bool(false),env)) else (Clos(Bool(true),env)) 
                        | _ -> raise Type_Error)
| Clos(Equals(exp1,exp2),env) ->( match ((krivine (Clos(exp1,env)) stack) , (krivine (Clos(exp2,env)) stack)) with 
                            (Clos(Integer(i1),envd1),Clos(Integer(i2),envd2)) -> if (i1 = i2) then (Clos(Bool(true),env)) else (Clos(Bool(false),env))
                            | _ -> raise Type_Error)
| Clos(GreaterTE(exp1,exp2),env) ->( match ((krivine (Clos(exp1,env)) stack) , (krivine (Clos(exp2,env)) stack)) with 
                            (Clos(Integer(i1),envd1),Clos(Integer(i2),envd2)) -> if (i1 >= i2) then (Clos(Bool(true),env)) else (Clos(Bool(false),env))
                            | _ -> raise Type_Error)
| Clos(LessTE(exp1,exp2),env) ->( match ((krivine (Clos(exp1,env)) stack) , (krivine (Clos(exp2,env)) stack)) with 
                            (Clos(Integer(i1),envd1),Clos(Integer(i2),envd2)) -> if (i1 <= i2) then (Clos(Bool(true),env)) else (Clos(Bool(false),env))
                            | _ -> raise Type_Error)
| Clos(GreaterT(exp1,exp2),env) ->( match ((krivine (Clos(exp1,env)) stack) , (krivine (Clos(exp2,env)) stack)) with 
                            (Clos(Integer(i1),envd1),Clos(Integer(i2),envd2)) -> if (i1 > i2) then (Clos(Bool(true),env)) else (Clos(Bool(false),env))
                            | _ -> raise Type_Error)
| Clos(LessT(exp1,exp2),env) ->( match ((krivine (Clos(exp1,env)) stack) , (krivine (Clos(exp2,env)) stack)) with 
                            (Clos(Integer(i1),envd1),Clos(Integer(i2),envd2)) -> if (i1 < i2) then (Clos(Bool(true),env)) else (Clos(Bool(false),env))
                            | _ -> raise Type_Error)
| Clos(Abs(exp1),env) -> (match (krivine (Clos(exp1,env)) stack) with 
                          (Clos(Integer(i),env1)) -> if (i<0) then (Clos(Integer(-i),env)) else (Clos(Integer(i),env)) 
                        | _ -> raise Type_Error)
| Clos(Negative(exp1),env) -> (match (krivine (Clos(exp1,env)) stack) with 
                          (Clos(Integer(i),env1)) -> (Clos(Integer(-i),env)) 
                          | _ -> raise Type_Error)
| Clos(Bool(b),env) -> cl
| Clos(Not(exp1),env) -> (match (krivine (Clos(exp1,env)) stack) with 
                          (Clos(Bool(b),env1)) -> (Clos(Bool(not b),env))
                        | _ -> raise Type_Error)
| Clos(And(exp1,exp2),env) ->  (match ((krivine (Clos(exp1,env)) stack) , (krivine (Clos(exp2,env)) stack)) with 
                                  (Clos(Bool(b1),envd1),Clos(Bool(b2),envd2)) -> Clos(Bool(b1 && b2),env)
                                  | _ -> raise Type_Error)
| Clos(Or(exp1,exp2),env) ->  (match ((krivine (Clos(exp1,env)) stack) , (krivine (Clos(exp2,env)) stack)) with 
                                (Clos(Bool(b1),envd1),Clos(Bool(b2),envd2)) -> Clos(Bool(b1 || b2),env)
                                | _ -> raise Type_Error)
| Clos(If_Then_Else(expb,exp1,exp2),env) -> (match (krivine (Clos(expb,env)) stack) with
                                    (Clos(Bool(true),envd1)) -> (Clos(exp1,env))
                                  | (Clos(Bool(false),envd1)) -> (Clos(exp2,env))
                                  | _ -> raise Type_Error)
| Clos(Lambda(exp1,exp2),env) -> (let absApplied (clx, s) = match (clx, s) with
                        | ((Clos(Lambda(V(str),expd), env1)), c::c') -> (Clos(expd, (update_table_krivine str c env1)), c')
                        | (_,[]) -> raise Type_Error
                                            | _ -> raise Type_Error in 
                                  let (cl', s') = absApplied (cl, stack) in
                                  krivine cl' s')
| Clos (App(exp1, exp2), env) -> krivine (Clos(exp1,env)) ((Clos(exp2,env))::stack)
| Clos (Paren(exp),env) -> krivine (Clos(exp,env)) stack
  
let rec execute_krivine exp table =  let cl = krivine (Clos(exp,table)) [] in 
  (match cl with 
  Clos(Integer(i), _ ) -> Number(i)
  | Clos(Bool(b) , _ ) -> Boolean(b)
  | _ -> raise Closure_Error
  )
  
let rec update_table_secd (str:string) (ans:answer) (env:table_secd) :table_secd = (let func (stra:string) = (match stra with
                                                                               str -> ans
                                                                              | _ -> (env str) )  in 
                                                              func)
  
let rec secd (stack:stack_secd) (env:table_secd) (opcodes:opcode list) (dump:dump) :answer = match (stack,env,opcodes,dump) with
  (x::s, _, [], _) -> x
  | (s, e, NCONST(i)::c, d) -> (secd (Num(i)::s)  e  c  d)
  | (s, e, BCONST(b)::c , d) -> (secd (BoolA(b)::s)  e  c  d)
  | (s, e, VAR(str)::c, d) -> (secd ((env str)::s) e c d)
  | (Num(i2)::(Num(i1)::s), e , PLUS::c , d ) -> (secd (Num(i1+i2)::s) e c d)
  | (Num(i2)::(Num(i1)::s), e , MINUS::c , d ) -> (secd (Num(i1-i2)::s) e c d)
  | (Num(i2)::(Num(i1)::s), e , MULT::c , d ) -> (secd (Num(i1*i2)::s) e c d)
  | (Num(i2)::(Num(i1)::s), e , DIV::c , d ) -> (secd (Num(i1/i2)::s) e c d)
  | (Num(i2)::(Num(i1)::s), e , REM::c , d ) -> (secd (Num(i1 mod i2)::s) e c d)
  | (Num(i2)::(Num(i1)::s), e , EQ::c , d ) -> (secd (BoolA(i1=i2)::s) e c d)
  | (Num(i2)::(Num(i1)::s), e , GTE::c , d ) -> (secd (BoolA(i1>=i2)::s) e c d)
  | (Num(i2)::(Num(i1)::s), e , LTE::c , d ) -> (secd (BoolA(i1<=i2)::s) e c d)
  | (Num(i2)::(Num(i1)::s), e , GT::c , d ) -> (secd (BoolA(i1>i2)::s) e c d)
  | (Num(i2)::(Num(i1)::s), e , LT::c , d ) -> (secd (BoolA(i1<i2)::s) e c d)
  | (Num(i1)::s, e , CMP::c , d) -> (let bans = if (i1 = 0) then true else false in  
                                    (secd (BoolA(bans)::s) e c d) ) 
  | (Num(i)::s, e , ABS::c , d) -> if i < 0 then ( secd  (Num(-i)::s) e c d) else ( secd  (Num(i)::s) e c d)
  | (Num(i)::s, e , NEG::c , d) -> ( secd  (Num(-i)::s) e c d)
  | (BoolA(b)::s, e , NOT::c , d) ->( secd  (BoolA(not b)::s) e c d)
  | (BoolA(b2)::(BoolA(b1)::s), e , AND::c , d) ->( secd  (BoolA(b1 && b2)::s) e c d)
  | (BoolA(b2)::(BoolA(b1)::s), e , ORR::c , d) ->( secd  (BoolA(b1 || b2)::s) e c d)
  | (exp2::(exp1::(BoolA(b)::s)), e, IFTE::c , d) -> if b then (secd (exp1::s) e c d) else (secd (exp2::s) e c d)
  | (s, e, CLOS(sx,oplist)::c , d) -> (secd (ValClosure(e,sx,oplist)::s)  e  c  d)
        (* Call by name : Now we evaluate c' first and store the function abstraction in dump *)
  | (x::ValClosure(e', x', c')::s, e, FCALL::c, d) -> (secd [] (update_table_secd x' x e') c' ((s, e, c)::d))
  | (a::s,e,RET::c,((sd,ed,cd)::d)) -> (secd (a::sd) ed cd d)
  | (a::s,e,PAREN::c,d) -> (secd (a::s) e c d)

let rec compile exp = match exp with 
    V(s) -> VAR(s)::[]
  | Lambda(V(s),exp1) -> [CLOS(s,(compile exp1)@[RET])] 
  | App(exp1,exp2) -> (compile exp1)@(compile exp2)@(FCALL::[]) 
  | Bool(b) -> BCONST(b)::[]
  | Integer(x) -> NCONST(x)::[]
  | Plus(exp1,exp2) -> (compile exp1)@(compile exp2)@(PLUS::[])
  | Sub(exp1,exp2) -> (compile exp1)@(compile exp2)@(MINUS::[])
  | Mult(exp1,exp2) -> (compile exp1)@(compile exp2)@(MULT::[])
  | Div(exp1,exp2) -> (compile exp1)@(compile exp2)@(DIV::[])
  | GreaterTE(exp1,exp2) -> (compile exp1)@(compile exp2)@(GTE::[])
  | LessTE(exp1,exp2) -> (compile exp1)@(compile exp2)@(LTE::[])
  | Equals(exp1,exp2) -> (compile exp1)@(compile exp2)@(EQ::[])
  | GreaterT(exp1,exp2) -> (compile exp1)@(compile exp2)@(GT::[])
  | LessT(exp1,exp2) -> (compile exp1)@(compile exp2)@(LT::[])
  | Rem(exp1,exp2) -> (compile exp1)@(compile exp2)@(REM::[])
  | Cmp(exp1) -> (compile exp1)@(CMP::[])
  | Abs(exp1) -> (compile exp1)@(ABS::[])
  | Negative(exp1) -> (compile exp1)@(NEG::[])
  | Not(exp1) -> (compile exp1)@(NOT::[])
  | And(bexp1,bexp2) -> (compile bexp1)@(compile bexp2)@(AND::[])
  | Or(bexp1,bexp2) -> (compile bexp1)@(compile bexp2)@(ORR::[])
  | If_Then_Else (exp1,exp2,exp3) -> (compile exp1)@(compile exp2)@(compile exp3)@(IFTE::[])
  | Paren(exp) -> (compile exp)@[PAREN]

let execute_secd expr table = secd [] table  (compile expr) []

let print_value tr = match tr with
  Number a -> Pervasives.print_string (string_of_int a)
  | Boolean a -> Pervasives.print_string (string_of_bool a)
  | _ -> raise Not_implemented

