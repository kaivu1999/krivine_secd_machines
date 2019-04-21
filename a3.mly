%{
    open A1
%}
/*
- Tokens (token name and rules) are modified wrt to A2. Please make necessary changes in A3
- LP and RP are left and right parenthesis
- Write grammar rules to recognize
  - >= <= from GT EQ LT tokens
  - if then else fi
*/
/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT CMP LT LP RP IF THEN ELSE FI COMMA PROJ
LET IN END BACKSLASH DOT DEF SEMICOLON PARALLEL LOCAL EOF EOL COLON TINT TBOOL TUNIT TTUPLE ARROW STAR

%nonassoc ABS TILDA NOT  

%left DISJ CONJ
%left EQ GT LT  
%left PLUS MINUS 
%left TIMES DIV REM COMMA

%start exp_parser 
%type <A1.expr> exp_parser /* Returns expression */
%%



/* ........................... */
exp_parser:
  exp_parser_0 EOF {$1}
|  exp_parser_0 EOL {$1}
;

exp_parser_0:
  exp_parser_0 DISJ main1 {Or($1,$3)}
  | main1 {$1}
;

main1:
   BACKSLASH ID DOT exp_parser_0 {Lambda(V($2),$4)}
  | exp_parser_0 LP exp_parser_0 RP {App($1,$3)}
  | main1 CONJ exp_parser_0 {And($1,$3)}
  | main2 {$1}
;

main2:
  NOT exp_parser_0 {Not($2)}
  | main3 {$1}
;

main3:
   CMP exp_parser_0 {Cmp($2)}
  | main3 EQ exp_parser_0 {Equals($1,$3)}
  | main3 GT exp_parser_0 {GreaterT($1,$3)}
  | main3 LT exp_parser_0 {LessT($1,$3)}
  | main3 GT EQ exp_parser_0 {GreaterTE($1,$4)}
  | main3 LT EQ exp_parser_0 {LessTE($1,$4)}
  | main4 {$1}
;

main4:
  main4 PLUS exp_parser_0 {Plus($1,$3)}
  | main4 MINUS exp_parser_0 {Sub($1,$3)}
  | main5 {$1}
;

main5:
  main5 DIV exp_parser_0 {Div($1,$3)}
  | main5 TIMES exp_parser_0 {Mult($1,$3)}
  | main5 REM exp_parser_0 {Rem($1,$3)}
  | main6 {$1}
;

main6:
  ABS exp_parser_0 {Abs($2)}
  | main7 {$1}
;

main7:
  TILDA exp_parser_0 {Negative($2)}
  | main8 {$1}
;

main8:
  IF exp_parser_0 THEN exp_parser_0 ELSE exp_parser_0 FI {If_Then_Else($2,$4,$6)}
  | main10 {$1}
;

main10:
  LP exp_parser_0 RP {Paren($2)}
  | main11 { $1 }
;

main11:
  BOOL {Bool($1)}
  | INT {Integer($1)}
  | ID {V($1)}
  /* | exp_parser_0 {$1} */
;
