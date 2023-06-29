%{
  open Syntax
%}

%token <int>    INT
%token <string> ID 
%token TRUE FALSE
%token LET REC AND IN
%token FUN RARROW
%token MATCH WITH END BAR
%token UNDERSCORE
%token IF THEN ELSE
%token COMMA
%token PLUS MINUS MUL DIV
%token EQ LT
%token DCOLON LBRK RBRK SEMI
%token LPAR RPAR 
%token EOF DSEMI

%start main 
%type <Syntax.command> main
%% 

main:
  | comm                { $1 }
  | EOF                 { raise End_of_file }
;

comm:
  | expr DSEMI                         { CExp $1} 
  | LET var eq_fun_expr DSEMI          { CLet ($2, $3)}
  | LET REC var eq_fun_expr DSEMI      { CRLet ($3, $4) }
  | LET REC var eq_fun_expr AND mutual_rec_fun DSEMI  { CMRLet (($3, $4) :: $6) }
;

expr:
  | IF expr THEN expr ELSE expr            { EIf ($2, $4, $6) }
  | LET var eq_fun_expr IN expr            { ELet ($2 ,$3, $5) }
  | LET REC var eq_fun_expr IN expr        { ERLet ($3, $4, $6) }
  | LET REC var eq_fun_expr AND mutual_rec_fun IN expr  { EMRLet ((($3, $4) :: $6), $8) }
  | FUN fun_expr                           { $2 }
  | MATCH expr WITH match_expr END         { EMatch ($2, $4) }  
  | comp_expr COMMA tuple_expr             { ETuple ($1, $3) } 
  | comp_expr                              { $1 }
;

mutual_rec_fun:
  | var eq_fun_expr AND mutual_rec_fun { ($1, $2) :: $4 }
  | var eq_fun_expr                    { ($1, $2) :: [] }
;

eq_fun_expr:
  | var eq_fun_expr           { EFun ($1, $2) }
  | EQ expr                   { $2 }
;

fun_expr:
  | var fun_expr              { EFun ($1, $2) }
  | var RARROW expr           { EFun ($1, $3) }
;

match_expr:
  | pattern RARROW expr       { [($1, $3)] }
  | pattern RARROW expr BAR match_expr { ($1, $3) :: $5 }
;

pattern:
  | list_pattern COMMA tuple_pattern      { PTuple ($1, $3) } 
  | list_pattern                          { $1 } 
;

tuple_pattern:
  | list_pattern COMMA tuple_pattern           { PTuple ($1, $3) }
  | list_pattern                               { PTuple ($1, PValue (VNil)) }
;

list_pattern:
  | simple_pattern DCOLON list_pattern         { PList ($1, $3) }
  | LBRK simplified_pattern_list RBRK     { $2 }
  | simple_pattern                        { $1 }
;

simplified_pattern_list:
  | pattern SEMI simplified_pattern_list { PList ($1, $3) }
  | pattern                              { PList ($1, PValue (VNil)) }
;

simple_pattern:
  | TRUE           { PValue (VBool true)}
  | FALSE          { PValue (VBool false)}
  | INT            { PValue (VInt $1) }
  | LBRK RBRK      { PValue (VNil) }
  | ID             { PVar   $1 }
  | UNDERSCORE     { PWild }
  | LPAR pattern RPAR { $2 }
;

tuple_expr:
  | comp_expr COMMA tuple_expr   { ETuple ( $1, $3) } 
  | comp_expr                    { ETuple ( $1, EValue VNil) } 
;

comp_expr:
  | comp_expr EQ list_expr   { EBin (OpEq, $1, $3) } 
  | comp_expr LT list_expr   { EBin (OpLt, $1, $3) } 
  | list_expr                { $1 } 
;

list_expr:
  | arith_expr DCOLON list_expr    { EList ($1, $3) }
  | arith_expr                { $1 } 
;

arith_expr:
  | arith_expr PLUS factor_expr { EBin (OpAdd,$1,$3) }
  | arith_expr MINUS factor_expr { EBin (OpSub,$1,$3) }
  | factor_expr                 { $1 }
;

factor_expr: 
  | factor_expr MUL app_expr { EBin (OpMul,$1,$3) }
  | factor_expr DIV app_expr { EBin (OpDiv,$1,$3) }
  | app_expr                 { $1 }
;

app_expr:
  | app_expr atomic_expr          { EApp ($1, $2) }
  | atomic_expr               { $1 }
;

atomic_expr:
  | TRUE           { EValue (VBool true)}
  | FALSE          { EValue (VBool false)}
  | INT            { EValue (VInt $1) }
  | ID             { EVar   $1 }
  | LPAR expr RPAR { $2 }
  | LBRK simplified_expr_list RBRK { $2 }
;

simplified_expr_list:
  | expr SEMI simplified_expr_list   { EList ($1, $3) }
  | expr                             { EList ($1, EValue (VNil)) }
  |                                  { EValue (VNil) }

var:
  | ID  { $1 } 
;