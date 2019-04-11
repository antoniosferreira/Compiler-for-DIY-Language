%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "node.h"
#include "tabid.h"
extern int yylex();
void yyerror(char *s);

typedef int bool;
#define true 1
#define false 0

#define MAX_PARAMS_ALLOWED 15
#define MAX_FUNCTIONS_ALLOWED 50

int functions_params[MAX_FUNCTIONS_ALLOWED][MAX_PARAMS_ALLOWED] = {{-1}};
int function_counter = 0;
int param_counter = 0;

void addParameter(int type) {
	functions_params[function_counter][param_counter] = type;
	param_counter++;}

int finalizeParameters() {
	int temp = function_counter;

	function_counter++;
        param_counter = 0;

	return temp;}

// Aids on getting parameter types on function calls
int expressions_params[MAX_PARAMS_ALLOWED] = {-1};
int exprs_counter = 0;

void addExpression(int type) {
	expressions_params[exprs_counter] = type;
	exprs_counter++;}

void finalizeExpression() {
	exprs_counter = 0;}


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

%nonassoc '!' dummyADDR dummyPTR dummyUNARY
%nonassoc INCR DECR

%nonassoc '(' '['

// Tree
%token PROG DECL DECLS FCALL ID BVARS BINSTRS BFULL PARAMS INTEGER NUMBER STRING VOID PTR NIL
%token INSTRS UPOS UMIN ADD MIN DIV MUL CEQ CNEQ CLT CGT CGE CLE ASS EXPS ALLOC STR NUM IDE
%token IF MEM CONINUE BREAK WHILE FOR FUNC ADDR MOD OR AND PTR INT MALLOC FACT NEG

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
            |   function 	{$$ = $1; $$->attrib = finalizeParameters();}
            ;


identifier	:	type ID					{$$ = uniNode(DECL, strNode(ID, $2)); declareVariable($1->info, $2, false, false);}
		|	type ID ATR expression			{$$ = binNode(IDE, strNode(ID, $2), $4); declareVariable($1->info, $2, false, false);}
		|	CONST type ID ATR expression		{$$ = binNode(IDE, strNode(ID, $3), $5); declareVariable($2->info, $3, true, false);}
		|	PUBLIC type ID				{$$ = uniNode(DECL, strNode(ID, $3)); declareVariable($2->info, $3, false, true);}
		|	PUBLIC type ID ATR expression		{$$ = binNode(IDE, strNode(ID, $3), $5); declareVariable($2->info, $3, false, true);}
		|	PUBLIC CONST type ID			{$$ = uniNode(DECL, strNode(ID, $4)); declareVariable($3->info, $4, true, true);}
		|	PUBLIC CONST type ID ATR expression	{$$ = binNode(IDE, strNode(ID, $4), $6); declareVariable($3->info, $4, true, true);}
		;


block_var	:	parameter		{$$ = $1;}
		|	type ID ATR expression 	{$$ = binNode(IDE, strNode(ID, $2), $4); declareVariable($1->info, $2, false, false);}
		;


function	:	function_def ';'	{$$ = $1;}
		|	function_decl ';' 	{$$ = $1;}
		;


function_def	:	PUBLIC type ID '(' parameters	')' block 	{$$ = triNode(FUNC, strNode(ID, $3), $5, $7); declareFunction($2->type, $3, true); IDpush();}
                |	PUBLIC type ID '(' ')' block			{$$ = binNode(FUNC, strNode(ID, $3), $6); declareFunction($2->type, $3, true); IDpush();}
                |	type ID '(' parameters ')' block		{$$ = triNode(FUNC, strNode(ID, $2), $4, $6); declareFunction($1->type, $2, false); IDpush();}
                |	type ID '(' ')' block 				{$$ = binNode(FUNC, strNode(ID, $2), $5); declareFunction($1->type, $2, false); IDpush();}
                ;


function_decl	:	PUBLIC type ID '(' parameters ')'	{$$ = binNode(FUNC, strNode(ID, $3), $5);}
                |	PUBLIC type ID '(' ')'			{$$ = uniNode(FUNC, strNode(ID, $3));}
                |	type ID '(' parameters ')'		{$$ = binNode(FUNC, strNode(ID, $2), $4);}
                |	type ID '(' ')'				{$$ = uniNode(FUNC, strNode(ID, $2));}
		;

function_call	:	ID '(' ')'			{$$ = uniNode(FCALL, strNode(ID, $1)); }
		|	ID '(' expressions ')'		{$$ = binNode(FCALL, strNode(ID, $1), $3);}
		;


block_vars	:	block_var ';'			{$$ = $1;}
		|	block_vars block_var ';'	{$$ = binNode(BVARS, $1, $2); $$->info = $1->info + $2->info;}
		;


parameters	:	parameter 			{addParameter($1->info); $$ = uniNode(PARAMS, $1); $$->info = $1->info;}
		|	parameters ',' parameter	{addParameter($3->info); $$ = binNode(PARAMS, $1, $3); $$->info = $1->info + $3->info;}
		;


parameter	:	type ID		{$$ = strNode(ID, $2); $$->info = $1->info;}
		;


type    :   	INTEGER		{$$ = uniNode(INTEGER, 0); $$->info = 1;}
        |   	NUMBER		{$$ = uniNode(NUMBER, 0); $$->info = 2;}
        |   	STRING		{$$ = uniNode(STRING, 0); $$->info = 3;}
        |	VOID		{$$ = uniNode(VOID, 0); $$->info = 0;}
        |	type '*'	{$$ = uniNode(PTR, 0); $$->info = calculatePointerType($1->info);}
        ;


block		:	'{' '}'						{IDpop(); $$ = nilNode(NIL);}
		|	'{' block_vars '}'				{IDpop(); $$ = uniNode(BVARS, $2);}
		|	'{' block_instructions '}'			{IDpop(); $$ = uniNode(BINSTRS, $2);}
		|	'{' block_vars block_instructions '}'		{IDpop(); $$ = binNode(BFULL, $2, $3); }
		;


block_instructions	:	instructions	{$$ = $1;}
			;


instructions	:	instruction 			{$$ = $1;}
		|	instructions instruction	{$$ = binNode(INSTRS, $1, $2);}
		;


instruction	:	expression ';'			{$$ = $1;}
		|	conditional	 		{$$ = $1;}
		|	BREAK INT ';'			{$$ = uniNode(BREAK, $$ = intNode(INT, $2));}
		|	CONTINUE INT ';'		{$$ = uniNode(CONTINUE, $$ = intNode(INT, $2));}
		|	BREAK ';'			{$$ = uniNode(BREAK, $$ = intNode(INT, 1));}
		|	CONTINUE ';'			{$$ = uniNode(CONTINUE, $$ = intNode(INT, 1));}
		|	loops 				{$$ = $1;}
		|	block 				{IDpush(); $$ = $1;}
		|	leftvalue '#' expression ';' 	{$$ = binNode(MALLOC, $1, $3);}
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
		|	INCR leftvalue				{$$ = binNode(ADD, $2, intNode(INT, 1)); $$->type = $2->type;}
		|	leftvalue INCR				{$$ = binNode(ADD, $1, intNode(INT, 1)); $$->type = $1->type;}
		|	DECR leftvalue				{$$ = binNode(MIN, $2, intNode(INT, 1)); $$->type = $2->type;}
		|	leftvalue DECR				{$$ = binNode(MIN, $1, intNode(INT, 1)); $$->type = $1->type;}
		|	expression '+' expression 		{$$ = binNode(ADD, $1, $3); $$->type = $1->type;}
		|	expression '-' expression		{$$ = binNode(MIN, $1, $3); $$->type = $1->type;}
		|	expression '/' expression		{$$ = binNode(DIV, $1, $3); $$->type = $1->type;}
		|	expression '*' expression		{$$ = binNode(MUL, $1, $3); $$->type = $1->type;}
		|	expression '%' expression		{$$ = binNode(MOD, $1, $3); $$->type = $1->type;}
		|	expression '=' expression		{$$ = binNode(CEQ, $1, $3); $$->type = $1->type;}
		|	expression NE expression		{$$ = binNode(CNEQ, $1, $3); $$->type = $1->type;}
		|	expression '<' expression		{$$ = binNode(CLT, $1, $3); $$->type = $1->type;}
		|	expression '>' expression		{$$ = binNode(CGT, $1, $3); $$->type = $1->type;}
		|	expression LE expression		{$$ = binNode(CLE, $1, $3); $$->type = $1->type;}
		|	expression GE expression		{$$ = binNode(CGE, $1, $3); $$->type = $1->type;}
		|	expression '|' expression		{$$ = binNode(OR, $1, $3); $$->type = $1->type;}
		|	expression '&' expression		{$$ = binNode(AND, $1, $3); $$->type = $1->type;}
		|	leftvalue				{$$ = $1;}
		|	leftvalue ATR expression		{$$ = binNode(ASS, $3, $1);  $$->type = $1->type;}
		|	'(' expression ')'			{$$ = $2;}
		|	'[' expression ']'			{$$ = $2;}
		|	'&' leftvalue %prec dummyADDR		{$$ = uniNode(ADDR, $2); $$->type = $2->type;}
                |	'*' leftvalue %prec dummyPTR		{$$ = uniNode(PTR, $2); $$->type = $2->type;}
		|	'+' expression %prec dummyUNARY		{$$ = uniNode(UPOS, $2); $$->type = $2->type;}
                |	'-' expression %prec dummyUNARY		{$$ = uniNode(UMIN, $2); $$->type = $2->type;}
		|	'~' expression				{$$ = uniNode(NEG, $2); $$->type = $2->type;}
		|	expression '!'				{$$ = uniNode(FACT, $1); $$->type = $1->type;}
		|	literal					{$$ = $1;}
		;


expressions	:	expression			{addExpression($1->info); $$ = uniNode(EXPS, $1);}
		|	expression ',' expressions	{addExpression($1->info); $$ = binNode(EXPS, $3, $1);}
		;


literal	:	STR	{$$ = strNode(STR, $1); $$->info = 2;}
	|	REAL	{$$ = realNode(NUM, $1); $$->info = 1;}
	|	INT	{$$ = intNode(INT, $1); $$->info = 0;}
	;


leftvalue	:	ID '[' expression ']'	{$$ = binNode(MEM, strNode(ID, $1), $3);}
		|	ID 			{$$ = strNode(ID, $1); $$->info = IDfind($1, 0);}
		;

%%
char **yynames =
#if YYDEBUG > 0
	(char**)yyname;
#else
	0;
#endif


int calculatePointerType(int type) {
	// void pointer
	if (type==0) return 7;
	// integer pointer
	if (type==1) return 4;
	// string pointer
	if (type==3) return 6;
	// number pointer
	if (type==2) return 5;
	return 0;
}


void declareVariable(int type, char* id,bool isConst, bool isPublic) {
	IDnew(type, id, 0);
}

void declareFunction(int type, char* id, bool isPublic) {
	IDnew(type, id, 0);
}

/*
void validateFunctionCall(char *id, int types[]) {
	long *fID = 0;

	// Gets the fID if function exists
	// Otherwise yyerror
	IDfind(id, fID);

	for (int i = 0; functions_params[fID][i] != -1 ; i++) {
		if (functions_params[fID][i] !=
	}

}
*/
