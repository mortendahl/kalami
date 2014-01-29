
(* returns the base-10 digits of number as a list, ie 'getdigits 12345' gives [5;4;3;2;1] *)
let rec getdigits number =
	if (=) 0 number then [] 
	else
		let rightmost_digit = number mod 10 in
		rightmost_digit :: (getdigits (number / 10))


(* inverse of 'getdigits', ie 'getnumber [5;4;3;2;1]' gives 12345 *)
let rec getnumber digits =
	match digits with
		[] -> 0
		| rightmost_digit :: rest ->
			(getnumber rest) * 10 + rightmost_digit


(* selects every 'interval' number, starting at 'offset' from 'digits',
   ie  'skip_select 2 [5;4;3;2;1] 1' gives [4;2]
   and 'skip_select 2 [5;4;3;2;1] 0' gives [5;3;1]   *)
let skip_select interval digits offset =
	let rec helper digits offset =
		match digits with
			[] -> []
			| ele::rest ->
				if (=) 0 offset then 
					ele :: (helper rest (interval - 1))
				else
					helper rest (offset - 1)
	in
	helper digits offset


(* projection of 'number' into 'arity' components, ie 'project 2 12345' gives [(2, 24); (1, 135)] *)
let project arity number =
	let digits = getdigits number in
	let rec helper component =
		if component < 1 then []
		else
			(component, getnumber (skip_select arity digits (component - 1))) :: (helper (component - 1))
	in
	helper arity