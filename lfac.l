/* README
Flex code that defines a lexer for a programming language. It recognizes various tokens, including keywords, identifiers, numbers, strings, 
and characters, and generates corresponding tokens for further processing by a parser implemented using Yacc.
*/



%{
#include <stdio.h>
#include "y.tab.h"
%}
%option noyywrap
%%
"-" {return minus;}
"eval" {return eval;}
"natural" {return natural;}
"real" {return real;}
"caracter" {return caracter;}
"sir" {return sir;}
"boolean" {return boolean;}
"clasa" {return clasa;}
"vector" {return vector;}
"main" {return main2;}
"daca" {return daca;}
"altfel" {return altfel;}
"pentru" {return pentru;}
"cat timp" {return catTimp;}
"executa" {return executa;}
"copiaza" {return copiaza;}
"compara" {return compara;}
"lungime" {return lungime;}
"concateneaza" {return concateneaza;}
"functie" {return functie;}
"def" {return def;}
"<"|">"|"=="|">="|"<=" {return comp;}

("false")|("true") {yylval.bulval=strdup(yytext); return bul;}
[a-zA-Z][a-zA-Z0-9]* {yylval.numevar=strdup(yytext); return nume;}
-?([1-9][0-9]*|0) {yylval.natval=atoi(yytext); return nr_nat;}
-?([1-9][0-9]*|0)\.([0-9]*) {yylval.realval=atof(yytext); return nr_real;}
\'[^ \']\' {yylval.caraval=yytext[1]; return cara;}
\"[^ \"]*\" {yylval.sirval=strdup(yytext); return str;}
"//"[^\n\r]* ;
[ \t\r] ;
\n {yylineno++;}
. {return yytext[0];}
