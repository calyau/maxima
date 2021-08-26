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
GNU General Public License for more details. */

lexer grammar mapleLexer ;

COMMENT		:	'#' .*? '\n' -> skip ;
WS		:	[ \t\r\n]+ -> skip ;
SEMICOLON	:	';' ;
ASSIGN		:	':=' ;
DOUBLE_COLON	:	'::' ;
COLON		:	':' ;
PLUS		:	'+' ;
SUBTRACT	:	'-' ;
MULT		:	'*' ;
EXP		:	'**' ;
SLASH		:	'/' ;
CARET		:	'^' ;
EXCLAM		:	'!' ;
EQ		:	'=' ;
LT		:	'<' ;
GT		:	'>' ;
LE		:	'<=' ;
GE		:	'>=' ;
NE		:	'<>' ;
NOT		:	'not' ;
AND		:	'and' ;
OR		:	'or' ;
ARROW		:	'->' ;
AT		:	'@' ;
REPEAT_COMPOSE	:	'@@' ;
AMPMUL		:	'&*' ;
//		:    ;'&' [ ^' ' '\t' '\r' '\n' '*' '&' '(' ')' '[' ']' '{' '}' ';' ':' '\'' '`' '#' ]+
//		:    ;{ AMPOP ( String.sub (lexeme lexbuf) 1 ( String.length (lexeme lexbuf) - 1 ) ) }
SEQ		:	'$' ;
ELLIPSE		:	'..' ;
DOT		:	'.' ;
COMMA		:	',' ;
LPAREN		:	'(' ;
RPAREN		:	')' ;
LBRACK		:	'[' ;
RBRACK		:	']' ;
LBRACE		:	'{' ;
RBRACE		:	'}' ;
BAR		:	'|' ;
UNDERSCORE	:	'_' ;
PERCENT		:	'%' ;
BACKSLASH	:	'\\' ;
QUESTIONMARK	:	'?' ;
SEQK		:	'seq' ;
BY		:	'by' ;
DO		:	'do' ;
DONE		:	'done' ;
ELIF		:	'elif' ;
ELSE		:	'else' ;
END		:	'end' ;
FI		:	'fi' ;
FOR		:	'for' ;
FROM		:	'from' ;
IF		:	'if' ;
IN		:	'in' ;
GLOBAL		:	'global' ;
LOCAL		:	'local' ;
OD		:	'od' ;
OPTION		:	'option' ;
PROC		:	'proc' ;
QUIT		:	'quit' ;
READ		:	'read' ;
SAVE		:	'save' ;
STOP		:	'stop' ;
THEN		:	'then' ;
TO		:	'to' ;
WHILE		:	'while' ;
UNION		:	'union' ;
INTERSECT	:	'intersect' ;
MINUS		:	'minus' ;
MOD		:	'mod' ;
INT		:	[0-9]+ ;
ID		:	LETTER (LETTER | [0-9])* ;
fragment		LETTER : [a-zA-Z_] ;
DOUBLEQUOTE	:	'"' .*? '"' ;
BACKQUOTE	:	'`' .*? '`' ;
QUOTE		:	'\'' .*? '\'' ;
