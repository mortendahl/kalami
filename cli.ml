open Evaluation
open Printing


let eval stmt =
	let unbounded_guesses = get_unbounded_guesses stmt in
	print_string "Unbound guesses:";
	print_string (String.concat " " unbounded_guesses);
	print_string "\n\n";
	if (List.length unbounded_guesses) > 0 then
			let projector = project (List.length unbounded_guesses) in
			let rec helper number = 
					let projection = projector number in
					let guesses = List.combine unbounded_guesses projection in
					let result = eval_stmt [] guesses stmt in
					match result with
							None ->
									helper (number + 1)
							| Some(_) ->
									result
			in
			helper 0
	else
			eval_stmt [] [] stmt

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