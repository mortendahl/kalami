%{

open Structure
open Lexer

%}


%token <string> STR
%token <int> INT
%token LPAREN RPAREN
%token PLUS MINUS EXP MULT DIV
%token EQ ASSIGN LT NOT OR AND
%token LET IN IF THEN OTHERWISE GUESS ACCEPT REJECT FROM TO
%token EOF

%right PLUS MINUS
%right MULT DIV
%right EXP

%left AND
%left OR
%left NOT


%start main
%type <Structure.statement> main

%%

main:
	EOF								{ raise Eof }
	| statement EOF					{ $1 }
;

identifier:
	STR			{ $1 }
;

expression:
	INT								{ ExprNumber($1) }
	| identifier					{ ExprVariable($1) }
	| expression MULT expression	{ ExprProduct($1, $3) }
	| expression DIV expression		{ ExprDivision($1, $3) }
	| expression PLUS expression	{ ExprSum($1, $3) }
	| expression MINUS expression	{ ExprSubtraction($1, $3) }
	| expression EXP expression		{ ExprPower($1, $3) }
	| LPAREN expression RPAREN		{ $2 }
;

condition:
	expression EQ expression		{ CndEqual($1, $3) }
	| expression LT expression		{ CndLessthan($1, $3) }
	| NOT condition					{ CndNegation($2) }
	| condition OR condition		{ CndOr($1, $3) }
	| condition AND condition		{ CndAnd($1, $3) }
;
	
statement:
	LET identifier ASSIGN expression IN statement					{ StmtLet($2, $4, $6) }
	| IF condition THEN statement OTHERWISE statement				{ StmtIf($2, $4, $6) }
	| GUESS identifier IN statement									{ StmtGuess($2, $4) }
	| GUESS identifier FROM expression IN statement					{ StmtGuessLowerBound($2, $4, $6) }
	| GUESS identifier FROM expression TO expression IN statement	{ StmtGuessLowerUpperBound($2, $4, $6, $8) }
	| ACCEPT														{ StmtAccept }
	| REJECT														{ StmtReject }
;
