open Printing

let _ =
	try
		let lexbuf = Lexing.from_channel stdin in		
		let stmt = Parser.main Lexer.token lexbuf in
	
		print_string "\n*** Statement ***\n\n";
		print_stmt stmt;
		print_string "\n\n"

	with 
		Lexer.Eof ->
			print_string "End of file\n";
			exit 0
		| Parsing.Parse_error ->
			print_string "Parsing error\n";
			exit 1