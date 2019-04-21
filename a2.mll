{
  open A3
  exception Eof
  exception Not_implemented
}

let whitespace = [' ' '\t']+
let digit = ['1'-'9']
let digits = ['0'-'9']
let sign = ['-' '+']
let integer = sign?((digit['0'-'9']*)|'0')
let alphabets = ['a'-'z' 'A'-'Z']
let schars = ['_' '''] 
let identifier = (alphabets)(alphabets|digits|schars)*


rule read = parse
   whitespace       { read lexbuf }
| [' ' '\t' '\n']  { read lexbuf }
| integer as i  { if (String.get i 0 = '+') then  INT (int_of_string (String.sub i 1 ((String.length i)-1)))
                  else INT(int_of_string i)}
| ";;"            { EOL }    (* to demarcate end of each expression *)
| 'T'  {BOOL(true)}
| 'F'  {BOOL(false)}
| "abs" {ABS}
| '~'  {TILDA}
| "not"  {NOT}
| '+'  {PLUS}
| '-'  {MINUS}
| '*'  {TIMES}
| "div" {DIV}
| "rem" {REM}
| '('  {LP}
| ')'  {RP}
| "/\\"  {CONJ}
| "\\/"  {DISJ}
| '='  {EQ}
| '>' {GT}
| '<' {LT}
| "cmp" {CMP}
| "if" {IF}
| "then" {THEN}
| "else" {ELSE}
| "fi" {FI}
| "let" {LET}
| "in" {IN}
| "end" {END}
| '\\' {BACKSLASH}
| '.' {DOT}
| "Tint" {TINT}
| "Tuint" {TUNIT}
| "Tbool" {TBOOL}
| "Ttuple" {TTUPLE}
| "->" {ARROW}
| identifier as i {ID(i)}
| _ as err {failwith ("InvalidChar "^(Char.escaped err))}
| eof   {EOF}