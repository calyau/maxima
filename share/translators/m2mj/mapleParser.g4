/* François Thomasset -- INRIA Rocquencourt -- Octobre 2001 *)

(* Translation from Maple to MuPad : syntaxic specification of maple *)

(*
Copyright © 2001-2002 François Thomasset, all rights reserved.
All of Dan Stanger's changes are Copyright © 2021 Dan Stanger, all rights reserved.
Copying is covered by the GNU General Public License (GPL).
 
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
 
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.
 
$Revision: 1.1.1.1 $
$Date: 2002/03/08 08:31:57 $

*/
grammar mapleParser ;

import mapleLexer ;

program : statseq EOF ;
// comments : COMMENT* -> skip ;
statseq : stat ((SEMICOLON|COLON) stat)* ; 
stat    : nameseq ASSIGN exprseq #AssignStat
	| nameseq ASSIGN PROC LPAREN parmseq RPAREN result_type* decls_proc options_of_proc* statseq END #ProcStat
	| exprseq #ExprStat
	| READ expr #ReadStat
	| SAVE name_string (COMMA name_string)* #StatStat
	| IF expr THEN statseq elif_clause? else_clause? FI #IfStat
	| for_stmt #ForStat
	| for_in_stmt #ForInStat
	| QUIT #Quit
	| STOP #Stop ;
for_stmt :
	| FOR name for_without_name
	| for_without_name ;
for_without_name :
	| FROM expr for_without_from
	| for_without_from ;
for_without_from :
	| BY expr TO expr for_body
	| TO expr BY expr for_body
	| TO expr for_body
	| BY expr for_body
	| for_body ;
for_in_stmt : FOR name IN expr for_body ;
for_body : WHILE expr DO statseq OD #ForBodyWhile
	| DO statseq OD #ForBodyDo ;
elif_clause : (ELIF expr THEN statseq)+ ;
else_clause : ELSE statseq ;
exprseq : expr (COMMA expr)* ;
expr    : expr ARROW expr #ArrowExpr
	| expr OR expr #OrOp
	| expr AND expr #AndOp
	| NOT expr #NotOp
	| expr OP=(CARET|EXP) expr #ExpoOp
	| expr OP=(MULT|SLASH) expr #MultOp
	| expr OP=(PLUS|SUBTRACT) expr #AddOp
	| PLUS expr UPLUS #UnaryPlus
	| SUBTRACT expr UMINUS #UnaryMinus
	| expr AMPOP expr #NeutralOp
	| expr AMPMUL expr #NeutralMulOp
	| expr (SEQ|SEQK) exprseq #SeqWithPrefix
	| LBRACK? (SEQ|SEQK) LPAREN exprseq RPAREN RBRACK? #SeqSansPrefix
	| PROC LPAREN parmseq RPAREN expr (COLON|SEMICOLON)? END #ProcExpr
	| expr OP=(LT|GT|LE|GE|NE|EQ) expr #BinaryRelOp
	| expr MOD expr #ModOp
	| expr ELLIPSE expr #IntervalExpr
	| expr OP=(INTERSECT|MINUS|UNION) expr #SetRelOp
	| expr EXCLAM #Factorial
	| QUOTE expr QUOTE #UnevaluatedExpr
	| LBRACK exprseq RBRACK #ListExpr
	| LBRACE exprseq RBRACE #SetExpr
	| name #NameExpr
	| STRING #StringExpr
	| name functional_operator #FunctionalOperatorExpr
	| INT #IntExpr
	| INT DOT INT #FloatExpr
	| INT DOT #FloatExpr
	| DOT INT #FloatExpr
	| LPAREN exprseq RPAREN #ParenExpr
	| expr AT expr #ComposeExpr
	| expr REPEAT_COMPOSE expr #RepeatComposeExpr ;
name    : name_string #NameString
	| name DOT INT #NameDotInt
	| name DOT STRING #NameDotString
	| name DOT LPAREN expr RPAREN #NameDotExpr
	| name LBRACK exprseq RBRACK #NameBracket ;
functional_operator :
	| LPAREN exprseq RPAREN (LBRACK exprseq RBRACK)?
	| functional_operator LPAREN exprseq RPAREN ;
parmseq : oneparm (COMMA oneparm)* ;
result_type : DOUBLE_COLON name_string SEMICOLON ;
oneparm : name 
	| name DOUBLE_COLON name_string ;
nameseq : name (COMMA name)* ;
decls_proc : globals_of_proc 
	| locals_of_proc 
	| locals_of_proc globals_of_proc 
	| globals_of_proc locals_of_proc  ;
locals_of_proc :
	| LOCAL nameseq SEMICOLON  ;
globals_of_proc :
	| GLOBAL nameseq SEMICOLON  ;
options_of_proc : OPTION nameseq SEMICOLON  ;
name_string :
          ID #Id
	| DOUBLEQUOTE #DoubleQuote
	| QUOTE #Quote
	| BACKQUOTE #BackQuote ;
