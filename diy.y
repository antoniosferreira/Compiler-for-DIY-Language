%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "node.h"
#include "tabid.h"
extern int yylex();
void yyerror(char *s);
%}

%union {
	int	i;   /* integer value */
	char	*s;  /* id or string literal */
	double	r;   /* number value (32bit) */
	Node	*n;  /* tree node */
}

%token <i> INT
%token <r> REAL
%token <s> ID STR
%token VOID INTEGER NUMBER STRING
%token PUBLIC CONST IF THEN ELSE
%token WHILE DO FOR IN
%token STEP UPTO DOWNTO BREAK CONTINUE
%token ATR INC INCR DECR

%nonassoc dummyELSE
%nonassoc ELSE

%right ATR
%left '|'
%left '&'
%nonassoc '~'
%left '=' NE
%left '<' '>' GE LE
%left '+' '-'
%left '*' '/' '%'

%nonassoc dummyADDR dummyPTR dummyUNARY
%nonassoc INCR DECR

%nonassoc '(' '['


// Tree
%token PROG DECL DECLS FCALL ID BVARS BINSTRS BFULL PARAMS INTEGER NUMBER STRING VOID PTR NIL
%token INSTRS UPOS UMIN ADD MIN DIV MUL CEQ CNEQ CLT CGT CGE CLE ASS EXPS ALLOC STR NUM IDE
%token IF MEM CONINUE BREAK WHILE FOR FUNC ADDR MOD OR AND PTR INT

%type<n> declarations declaration identifier type function_call expressions block block_vars block_var
%type<n> parameters parameter block_instructions instructions instruction expression expressions
%type<n> conditional loops leftvalue literal function function_def function_decl identifier

%%

file	:	declarations	{printNode(uniNode(DECLS, $1), 0, yynames);}
	|
	;


declarations    :   declaration			{$$ = $1;}
                |   declarations declaration	{$$ = binNode(DECLS, $1, $2);}
                ;


declaration :   identifier ';'	{$$ = $1;}
            |   function 	{$$ = $1;}
            ;


identifier	:	type ID					{$$ = uniNode(DECL, strNode(ID, $2));}
		|	type ID ATR expression			{$$ = binNode(IDE, strNode(ID, $2), $4);}
		|	CONST type ID ATR expression		{$$ = binNode(IDE, strNode(ID, $3), $5);}
		|	PUBLIC type ID				{$$ = uniNode(DECL, strNode(ID, $3));}
		|	PUBLIC type ID ATR expression		{$$ = binNode(IDE, strNode(ID, $3), $5);}
		|	PUBLIC CONST type ID			{$$ = uniNode(DECL, strNode(ID, $4));}
		|	PUBLIC CONST type ID ATR expression	{$$ = binNode(IDE, strNode(ID, $4), $6);}
		;


block_var	:	parameter		{$$ = $1;}
		|	type ID ATR expression 	{$$ = binNode(IDE, strNode(ID, $2), $4);}
		;


function	:	function_def ';'	{$$ = $1;}
		|	function_decl ';' 	{$$ = $1;}
		;


function_def	:	PUBLIC type ID '(' parameters	')' block 	{$$ = triNode(FUNC, strNode(ID, $3), $5, $7);}
                |	PUBLIC type ID '(' ')' block			{$$ = binNode(FUNC, strNode(ID, $3), $6);}
                |	type ID '(' parameters ')' block		{$$ = triNode(FUNC, strNode(ID, $2), $4, $6);}
                |	type ID '(' ')' block 				{$$ = binNode(FUNC, strNode(ID, $2), $5);}
                ;


function_decl	:	PUBLIC type ID '(' parameters	')'	{$$ = binNode(FUNC, strNode(ID, $3), $5);}
                |	PUBLIC type ID '(' ')'			{$$ = uniNode(FUNC, strNode(ID, $3));}
                |	type ID '(' parameters ')'		{$$ = binNode(FUNC, strNode(ID, $2), $4);}
                |	type ID '(' ')'				{$$ = uniNode(FUNC, strNode(ID, $2));}
		;

function_call	:	ID '(' ')'			{$$ = uniNode(FCALL, strNode(ID, $1));}
		|	ID '(' expressions ')'		{$$ = binNode(FCALL, strNode(ID, $1), $3);}
		;


block_vars	:	block_var ';'			{$$ = $1;}
		|	block_vars block_var ';'	{$$ = binNode(BVARS, $1, $2); $$->info = $1->info + $2->info;}
		;


parameters	:	parameter 			{$$ = uniNode(PARAMS, $1); $$->info = $1->info;}
		|	parameters ',' parameter	{$$ = binNode(PARAMS, $1, $3); $$->info = $1->info + $3->info;}
		;


parameter	:	type ID		{$$ = strNode(ID, $2);}
		;


type    :   	INTEGER		{$$ = uniNode(INTEGER, 0); $$->info = 0;}
        |   	NUMBER		{$$ = uniNode(NUMBER, 0); $$->info = 1;}
        |   	STRING		{$$ = uniNode(STRING, 0); $$->info = 2;}
        |	VOID		{$$ = uniNode(VOID, 0); $$->info = 3;}
        |	type '*'	{$$ = uniNode(PTR, 0); $$->info = 10 + $1->info;}
        ;


block		:	'{' '}'						{$$ = nilNode(NIL);}
		|	'{' block_vars '}'				{$$ = uniNode(BVARS, $2);}
		|	'{' block_instructions '}'			{$$ = uniNode(BINSTRS, $2);}
		|	'{' block_vars block_instructions '}'		{$$ = binNode(BFULL, $2, $3); }
		;


block_instructions	:	instructions	{$$ = $1;}
			;


instructions	:	instruction 			{$$ = $1;}
		|	instructions instruction	{$$ = binNode(INSTRS, $1, $2);}
		;


instruction	:	expression ';'		{$$ = $1;}
		|	conditional	 	{$$ = $1;}
		|	BREAK INT ';'		{$$ = uniNode(BREAK, $$ = intNode(INT, $2));}
		|	CONTINUE INT ';'	{$$ = uniNode(CONTINUE, $$ = intNode(INT, $2));}
		|	BREAK ';'		{$$ = uniNode(BREAK, $$ = intNode(INT, 1));}
		|	CONTINUE ';'		{$$ = uniNode(CONTINUE, $$ = intNode(INT, 1));}
		|	loops 			{$$ = $1;}
		|	block 			{$$ = $1;}
		;


conditional	:	IF expression THEN instruction %prec dummyELSE	{$$ = binNode(IF, $2, $4);}
		|	IF expression THEN instruction ELSE instruction	{$$ = triNode(IF, $2, $4, $6);}
		;


loops	:	DO instruction WHILE expression ';'						{$$ = binNode(WHILE, $2, $4);}
	|	FOR leftvalue IN expression UPTO expression DO instruction			{$$ = quadNode(FOR, $2, $4, $6, $8);}
	|	FOR leftvalue IN expression DOWNTO expression DO instruction			{$$ = quadNode(FOR, $2, $4, $6, $8);}
	|	FOR leftvalue IN expression UPTO expression STEP expression DO instruction	{$$ = pentNode(FOR, $2, $4, $6, $8, $10);}
	|	FOR leftvalue IN expression DOWNTO expression STEP expression DO instruction	{$$ = pentNode(FOR, $2, $4, $6, $8, $10);}
	;



expression	:	function_call				{$$ = $1;}
		|	INCR leftvalue				{$$ = binNode(ADD, $2, intNode(INT, 1));}
		|	leftvalue INCR				{$$ = binNode(ADD, $1, intNode(INT, 1));}
		|	DECR leftvalue				{$$ = binNode(MIN, $2, intNode(INT, 1));}
		|	leftvalue DECR				{$$ = binNode(MIN, $1, intNode(INT, 1));}
		|	expression '+' expression 		{$$ = binNode(ADD, $1, $3);}
		|	expression '-' expression		{$$ = binNode(MIN, $1, $3);}
		|	expression '/' expression		{$$ = binNode(DIV, $1, $3);}
		|	expression '*' expression		{$$ = binNode(MUL, $1, $3);}
		|	expression '%' expression		{$$ = binNode(MOD, $1, $3);}
		|	expression '=' expression		{$$ = binNode(CEQ, $1, $3);}
		|	expression NE expression		{$$ = binNode(CNEQ, $1, $3);}
		|	expression '<' expression		{$$ = binNode(CLT, $1, $3);}
		|	expression '>' expression		{$$ = binNode(CGT, $1, $3);}
		|	expression LE expression		{$$ = binNode(CLE, $1, $3);}
		|	expression GE expression		{$$ = binNode(CGE, $1, $3);}
		|	expression '|' expression		{$$ = binNode(OR, $1, $3);}
		|	expression '&' expression		{$$ = binNode(AND, $1, $3);}
		|	leftvalue				{$$ = $1;}
		|	leftvalue ATR expression		{$$ = binNode(ASS, $3, $1);}
		|	'(' expression ')'			{$$ = $2;}
		|	'[' expression ']'			{$$ = $2;}
		|	'&' leftvalue %prec dummyADDR		{$$ = uniNode(ADDR, $2);}
                |	'*' leftvalue %prec dummyPTR		{$$ = uniNode(PTR, $2);}
		|	'+' expression %prec dummyUNARY		{$$ = uniNode(UPOS, $2);}
                |	'-' expression %prec dummyUNARY		{$$ = uniNode(UMIN, $2);}
		|	literal					{$$ = $1;}
		;


expressions	:	expression			{$$ = uniNode(EXPS, $1);}
		|	expression ',' expressions	{$$ = binNode(EXPS, $3, $1);}
		;


literal	:	STR	{$$ = strNode(STR, $1); $$->info = 2;}
	|	REAL	{$$ = realNode(NUM, $1); $$->info = 1;}
	|	INT	{$$ = intNode(INT, $1); $$->info = 0;}
	;


leftvalue	:	ID '[' expression ']'	{$$ = binNode(MEM, strNode(ID, $1), $3);}
		|	ID 			{$$ = strNode(ID, $1);}
		;

%%
char **yynames =
#if YYDEBUG > 0
	(char**)yyname;
#else.
	0;
#endif
