%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "node.h"
#include "tabid.h"
extern int yylex();
void yyerror(char *s);


int returnT;

/* Messy stuff to deal with storing info
 from functions parameters */
#define MAX_PARAMS_ALLOWED 15
#define MAX_FUNCTIONS_ALLOWED 40
int functions_params[MAX_FUNCTIONS_ALLOWED][MAX_PARAMS_ALLOWED] = {{}};
int function_counter = 0;
int old_function = -1;
int param_counter = 0;

// Adds a parameter type to the function is evaluating
void addParameter(int type, char *id) {
    if (old_function != function_counter) {
    	IDpush();
        old_function = function_counter;
    }

	functions_params[function_counter][param_counter] = type;
	param_counter++;}

// Gets everything ready for next function evaluation
int finalizeParameters() {
	int temp = function_counter;
	function_counter++;
	param_counter = 0;
	return temp;
}


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
%token IF MEM CONINUE BREAK WHILE FOR FUNC ADDR MOD OR AND PTR INT MALLOC FACT NEG IFBLOCK
%token BODY GLOBAL

%type<n> declarations declaration identifier type function_call expressions block block_vars block_var
%type<n> parameters parameter block_instructions instructions instruction expression expressions
%type<n> conditional loops leftvalue literal function function_def function_decl identifier conditional_block

%%


file :   declarations    {printNode(uniNode(DECLS, $1), 0, yynames);}
     |
     ;


declarations    :   declaration                 {$$ = $1;}
                |   declarations declaration	{$$ = binNode(DECLS, $1, $2);}
                ;


declaration :   identifier ';'      {$$ = $1;}
            |   function            {$$ = $1;}
            ;


identifier	:	type ID                             {$$ = uniNode(ID, strNode(ID, $2)); IDnew($1->info, $2, 0);}
		    |	type ID ATR expression              {$$ = binNode(ATR, $4, strNode(ID, $2)); IDnew($1->info, $2, 0); checkAttributionTypes($1->info, $4->info);}
		    |	CONST type ID ATR expression        {$$ = binNode(ATR, $5, strNode(ID, $3)); IDnew($2->info + 2000, $3, 0); checkAttributionTypes($2->info - 2000, $5->info);}
		    |	PUBLIC type ID                      {$$ = uniNode(ID, strNode(ID, $3)); IDnew($2->info, $3, 0);}
		    |	PUBLIC type ID ATR expression       {$$ = binNode(ATR, $5, strNode(ID, $3)); IDnew($2->info, $3, 0); checkAttributionTypes($2->info, $5->info);}
		    |	PUBLIC CONST type ID                {$$ = uniNode(ID, strNode(ID, $4)); IDnew($3->info + 2000, $4, 0);}
		    |	PUBLIC CONST type ID ATR expression	{$$ = binNode(ATR, $6, strNode(ID, $4)); IDnew($3->info + 2000, $4, 0); checkAttributionTypes($3->info - 2000, $6->info);}
		    ;


block_var   :	type ID                     {$$ = uniNode(ID, strNode(ID, $2)); IDnew($1->info, $2, 0);}
            |	type ID ATR expression      {$$ = binNode(ATR, $4, uniNode(ID, strNode(ID, $2))); $$->info = checkAttributionTypes($1->info, $4->info);}
            ;


function    :   function_def ';'        {$$ = $1; $$->fId = finalizeParameters();}
            |   function_decl ';'       {$$ = $1; $$->fId = finalizeParameters();}
		    ;


function_def	:	PUBLIC type ID '(' parameters ')' block 	{IDpop(); $$ = binNode(FUNC, strNode(ID, $3), $7); checkReturnType($2->info);}
                |	PUBLIC type ID '(' ')' block			    {IDpop(); $$ = binNode(FUNC, strNode(ID, $3), $6); checkReturnType($2->info);}
                |	type ID '(' parameters ')' block		    {IDpop(); $$ = binNode(FUNC, strNode(ID, $2), $6); checkReturnType($1->info);}
                |	type ID '(' ')' block 				        {IDpop(); $$ = binNode(FUNC, strNode(ID, $2), $5); checkReturnType($1->info);}
                ;


function_decl	:	PUBLIC type ID '(' parameters ')'	{IDpop(); IDnew($2->info + 1000, $3, 0); $$ = binNode(FUNC, strNode(ID, $3), NULL);}
                |	PUBLIC type ID '(' ')'			    {IDpop(); IDnew($2->info + 1000, $3, 0); $$ = binNode(FUNC, strNode(ID, $3), NULL);}
                |	type ID '(' parameters ')'		    {IDpop(); IDnew($1->info + 1000, $2, 0); $$ = binNode(FUNC, strNode(ID, $2), NULL);}
                |	type ID '(' ')'				        {IDpop(); IDnew($1->info + 1000, $2, 0); $$ = binNode(FUNC, strNode(ID, $2), NULL);}
		        ;


function_call	:   ID '(' ')'                  {$$ = binNode(FCALL, strNode(ID, $1), NULL); validateFunctionCall($1, NULL);}
                |	ID '(' expressions ')'      {$$ = binNode(FCALL, strNode(ID, $1), $3); validateFunctionCall($1, $3);}
                ;


block_vars	:   block_var ';'                   {$$ = $1;}
            |	block_vars block_var ';'        {$$ = binNode(BVARS, $1, $2);}
            ;


parameters	:	parameter                   {$$ = uniNode(PARAMS, $1); $$->info = $1->info;}
		    |	parameters ',' parameter    {$$ = binNode(PARAMS, $1, $3);}
		    ;


parameter	:	type ID     {addParameter($1->info, $2); $$ = strNode(ID, $2); $$->info = $1->info; IDnew($1->info, $2, 0); /* Apparently evrth is global for this delivery, but who knows */}
            ;


type    :   	INTEGER		{$$ = uniNode(INTEGER, 0); $$->info = 2;}
        |   	NUMBER		{$$ = uniNode(NUMBER, 0); $$->info = 3;}
        |   	STRING		{$$ = uniNode(STRING, 0); $$->info = 4;}
        |       VOID		{$$ = uniNode(VOID, 0); $$->info = 1;}
        |       type '*'	{$$ = uniNode(PTR, 0); $$->info = calculatePointerType($1->info);}
        ;


block	:	'{' '}'						            { $$ = nilNode(NIL);}
		|	'{' block_vars '}'				        { $$ = uniNode(BVARS, $2);}
		|	'{' block_instructions '}'			    { $$ = uniNode(BINSTRS, $2);}
		|	'{' block_vars block_instructions '}'	{ $$ = binNode(BFULL, $2, $3); }
		;


block_instructions	:	instructions	{$$ = $1;}
			        ;


instructions	:	instruction 			    {$$ = $1;}
		        |	instructions instruction	{$$ = binNode(INSTRS, $1, $2);}
		        ;


instruction	:	expression ';'                  {$$ = $1;}
		    |	conditional                     {$$ = $1;}
		    |	BREAK INT ';'                   {$$ = uniNode(BREAK, $$ = intNode(INT, $2));}
		    |	CONTINUE INT ';'                {$$ = uniNode(CONTINUE, $$ = intNode(INT, $2));}
	    	|	BREAK ';'                       {$$ = uniNode(BREAK, $$ = intNode(INT, 1));}
	    	|	CONTINUE ';'                    {$$ = uniNode(CONTINUE, $$ = intNode(INT, 1));}
	    	|	loops                           {$$ = $1;}
	    	|	block                           {$$ = $1;}
	    	|	leftvalue '#' expression ';'    {$$ = binNode(MALLOC, $1, $3); $$->info = $1->info;}
	    	;


conditional	:	IF expression conditional_block      {$$ = binNode(IF, $2, $3);}
	    	;


conditional_block   : THEN instruction %prec dummyELSE  {$$ = uniNode(IFBLOCK, $2);}
                    | THEN instruction ELSE instruction {$$ = binNode(IFBLOCK, $2, $4);}
                    ;


loops	:	DO instruction WHILE expression ';'                                             {$$ = binNode(WHILE, $2, $4);}
        |	FOR leftvalue IN expression UPTO expression DO instruction                      {$$ = quadNode(FOR, $2, $4, $6, $8); /*nicht so gut*/}
	    |	FOR leftvalue IN expression DOWNTO expression DO instruction                    {$$ = quadNode(FOR, $2, $4, $6, $8); /*nicht so gut*/}
	    |	FOR leftvalue IN expression UPTO expression STEP expression DO instruction      {$$ = pentNode(FOR, $2, $4, $6, $8, $10); /*nicht so gut*/}
	    |	FOR leftvalue IN expression DOWNTO expression STEP expression DO instruction    {$$ = pentNode(FOR, $2, $4, $6, $8, $10); /*nicht so gut*/}
	    ;



expression	:	function_call                       {$$ = $1;}
            |	INCR leftvalue                      {$$ = binNode(ADD, $2, intNode(INT, 1)); $$->info = $2->info;}
            |	leftvalue INCR                      {$$ = binNode(ADD, $1, intNode(INT, 1)); $$->info = $1->info;}
            |	DECR leftvalue                      {$$ = binNode(MIN, $2, intNode(INT, 1)); $$->info = $2->info;}
            |	leftvalue DECR                      {$$ = binNode(MIN, $1, intNode(INT, 1)); $$->info = $1->info;}
            |	expression '+' expression           {$$ = binNode(ADD, $1, $3); $$->info = checkBinTypes($1->info, $3->info);}
            |	expression '-' expression           {$$ = binNode(MIN, $1, $3); $$->info = checkBinTypes($1->info, $3->info);}
            |	expression '/' expression           {$$ = binNode(DIV, $1, $3); $$->info = checkBinTypes($1->info, $3->info);}
            |	expression '*' expression           {$$ = binNode(MUL, $1, $3); $$->info = checkBinTypes($1->info, $3->info);}
            |	expression '%' expression           {$$ = binNode(MOD, $1, $3); $$->info = checkBinTypes($1->info, $3->info);}
            |	expression '=' expression           {$$ = binNode(CEQ, $1, $3); $$->info = checkBinTypes($1->info, $3->info);}
            |	expression NE expression            {$$ = binNode(CNEQ, $1, $3);$$->info = checkBinTypes($1->info, $3->info);}
            |	expression '<' expression           {$$ = binNode(CLT, $1, $3); $$->info = checkBinTypes($1->info, $3->info);}
            |	expression '>' expression           {$$ = binNode(CGT, $1, $3); $$->info = checkBinTypes($1->info, $3->info);}
            |	expression LE expression            {$$ = binNode(CLE, $1, $3); $$->info = checkBinTypes($1->info, $3->info);}
            |	expression GE expression            {$$ = binNode(CGE, $1, $3); $$->info = checkBinTypes($1->info, $3->info);}
            |	expression '|' expression           {$$ = binNode(OR, $1, $3); $$->info = checkBinTypes($1->info, $3->info);}
            |	expression '&' expression           {$$ = binNode(AND, $1, $3); $$->info = checkBinTypes($1->info, $3->info);}
            |	leftvalue                           {$$ = $1;}
            |	leftvalue ATR expression            {$$ = binNode(ASS, $3, $1); $$->info = checkAttributionTypes($1->info, $3->info);}
            |	'(' expression ')'                  {$$ = $2;}
            |	'[' expression ']'                  {$$ = $2;}
            |	'&' leftvalue %prec dummyADDR       {$$ = uniNode(ADDR, $2); $$->info = $2->info;}
            |	'*' leftvalue %prec dummyPTR        {$$ = uniNode(PTR, $2); $$->info = $2->info;}
            |	'+' expression %prec dummyUNARY     {$$ = uniNode(UPOS, $2); $$->info = $2->info;}
            |	'-' expression %prec dummyUNARY     {$$ = uniNode(UMIN, $2); $$->info = $2->info;}
            |	'~' expression                      {$$ = uniNode(NEG, $2); $$->info = $2->info;}
            |	expression '!'                      {$$ = uniNode(FACT, $1); $$->info = $1->info;}
            |	literal                             {$$ = $1;}
            ;


expressions	:	expression                      {$$ = uniNode(EXPS, $1);}
		    |	expression ',' expressions      {$$ = binNode(EXPS, $3, $1);}
		    ;


literal	:	STR     {$$ = strNode(STR, $1); $$->info = 4;}
	    |	REAL    {$$ = realNode(NUM, $1); $$->info = 3;}
	    |	INT     {$$ = intNode(INT, $1); $$->info = 2;}
	    ;


leftvalue	:	ID '[' expression ']'   {$$ = binNode(MEM, strNode(ID, $1), $3);}
		    |	ID                      {$$ = strNode(ID, $1); $$->info = IDfind($1, (long*)IDtest); }
		    ;


%%

char **yynames =
#if YYDEBUG > 0
	(char**)yyname;
#else
	0;
#endif

void validateFunctionCall(char *id, Node* params) {
	long *fID = 0;

	// Gets the fID if function exists
	// Otherwise yyerror
	IDfind(id, fID);
	int f = (int)fID;

	// Ugly way to get the number of expected parameters
	int nparams = 0;
	for (int i = 0; i < MAX_PARAMS_ALLOWED; i++) {
		if (functions_params[f][i] != 0)
			nparams++;
	}

	// No arguments received
	if (params == NULL) {
		if (nparams == 0) return;
		yyerror("Missing arguments. Check function declaration.");
	} else {

		// Checks if arguments received match
		// With parameters
		int nargs = 0;
		int i = 0;

		// Aid: 314 node->attrib == 314 Avaliates if it is an expression node or literal
		for (Node* node = params; node->attrib == 314; node = node->value.sub.n[0]) {
			if (i > nparams-1) yyerror("More arguments provided than expected. Check function declaration.");

			else if ((node->value.sub.n[1] != NULL) && (node->value.sub.n[1]->info != functions_params[f][i]))
				yyerror("Wrong arguments type. Check function declaration.");
			else if ((node->value.sub.n[1] == NULL) && (node->value.sub.n[0]->info != functions_params[f][i]))
				yyerror("Wrong arguments type. Check function declaration.");

			i++;
			nargs++;
		}
		if (nargs < nparams) yyerror("Missing arguments in function call;");
	}
}

//      TYPE CHECKING   -----------------------------------

/* This function receives two types
   And evaluates if both are valid in an
   expression context;
   Returns the resultant <expression> type */
int checkBinTypes(int n1, int n2) {

    // Both have the same type? Good for them!
    if (n1 == n2) return n1;

    // Casting integer to number
    if ((n1 == 2 && n2 == 3)
        || (n1 == 3 && n2 == 2))
        return 3;

    yyerror("Wrong type of arguments.");
    // 0 is void
    return 0;
}

/* This function receives two types
 * and confirms the validity of
 * the attribution operation*/
int checkAttributionTypes(int n1, int n2) {

	//it's a function
	if (n1 == -1) {
		returnT = n2;
		return n2;
	}

    if (n1 == n2) return n1;

    // an integer can be a number too
    //if (n1 == 3 && n2 ==2) return n1;


    // If var was constant, will count as invalid too!
    yyerror("Invalid attribution.");
    //0 is void
    return 0;
}


/* Simple function to aid on
 *  resolving pointers types*/
int calculatePointerType(int type) {
    // void pointer
    if (type==1) return 8;
    // integer pointer
    if (type==2) return 5;
    // string pointer
    if (type==4) return 7;
    // number pointer
    if (type==3) return 6;
    return 0;
}

void checkReturnType(int type) {
	if (type != returnT) yyerror("Variable undeclared or problem with return type;");
}