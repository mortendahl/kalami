{

open Parser
exception Eof

}

rule token = parse

	[' ' '\t']				{ token lexbuf }    	(* skip blanks *)
	| ('\r')? '\n'			{ token lexbuf }		(* skip line breaks *)
	| "(*"					{ comments 0 lexbuf }	(* start comment parsing on level 0 *)

	| '('					{ LPAREN }
	| ')'					{ RPAREN }
	| '+'					{ PLUS }
	| '-'					{ MINUS }
	| "**"					{ EXP }
	| '*'					{ MULT }
	| '/'					{ DIV }
	
	| "=="					{ EQ }
	| '='					{ ASSIGN }
	| '<'					{ LT }
	| "not"					{ NOT }
	| "or"					{ OR }
	| "and"					{ AND }
	
	| "let"					{ LET }
	| "in"					{ IN }
	| "if"					{ IF }
	| "then"				{ THEN }
	| "else"				{ ELSE }
	| "guess"				{ GUESS }
	| "reject"				{ REJECT }
	| "accept"				{ ACCEPT }
	| "from"				{ FROM }
	| "to"					{ TO }
	
	| (['0'-'9']+ as i)		{ INT(int_of_string i) }
	| (['a'-'z' 'A'-'Z']+ as s1) (['a'-'z' 'A'-'Z' '0'-'9']* as s2)		{ STR(s1 ^ s2) }
	
	| '@'					{ EOF }
	| eof					{ EOF }
	
	
and comments level = parse

	| "*)"			{ if level = 0 then token lexbuf else comments (level-1) lexbuf }	(* stop comment parsing for this level and return to level n-1 (or to token parsing if level 0) *)
	| "(*"			{ comments (level+1) lexbuf }										(* start comment parsing on level n+1 *)
	| _				{ comments level lexbuf }
	| eof			{ raise Eof }
