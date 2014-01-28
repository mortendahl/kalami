open Evaluation
open Printing

let _ =
	try
		let lexbuf = Lexing.from_channel stdin in		
		let stmt = Parser.main Lexer.token lexbuf in
	
		print_string "\n*** Statement ***\n\n";
		print_stmt stmt;

		print_string "\n\n\n*** Solution ***\n\n";
		print_string "Looking for a solution (succeeding values)..\n\n";
		
		flush stdout;

		let result = eval stmt in
		match result with
            None ->
			    print_string "No solution exist\n";
			    flush stdout
		    | Some(solution) ->
		        print_string "Found a solution: \n\n";
		        print_env (List.rev solution);
		        print_string "\n";
		        flush stdout
	
	with Lexer.Eof ->
		print_string "End\n";
		exit 0