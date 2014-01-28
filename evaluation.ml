
open Structure
open Printing


    

(* ********** Diverse helper functions ********** *)


(* repeated squaring *)
let power base exponent =
    let rec helper base exponent acc =
        match exponent with
            0 -> acc
            | 1 -> base * acc
            | n ->
                let base' = base * base in
                let exponent' = exponent / 2 in (* integer division, rounding down *)
                if n mod 2 == 0 then
                    (* even *)
                    helper base' exponent' acc
                else
                    (* odd *)
                    let acc' = acc * base in
                    helper base' exponent' acc'
    in
    helper base exponent 1




(* projecting a number into a tuple with specified arity *)
let project arity number =
    
    (* returns the base-10 digits of number as a list, ie 'getdigits 12345' gives [5;4;3;2;1] *)
    let rec getdigits number =
        if (=) 0 number then [] 
        else
            let rightmost_digit = number mod 10 in
            rightmost_digit :: (getdigits (number / 10))
    in
    
    (* inverse of 'getdigits', ie 'getnumber [5;4;3;2;1]' gives 12345 *)
    let rec getnumber digits =
        match digits with
            [] -> 0
            | rightmost_digit :: rest ->
                (getnumber rest) * 10 + rightmost_digit
    in
    
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
    in
    
    (* projection of 'number' into 'arity' components, ie 'project 2 12345' gives [24; 135] *)
    let digits = getdigits number in
    let rec helper component =
        if component < 1 then []
        else
            (getnumber (skip_select arity digits (component - 1))) :: (helper (component - 1))
    in
    helper arity




(* finds the identifiers for guesses without an upper bound in a statement *)
let rec get_unbounded_guesses stmt =
    match stmt with
        StmtLet(_, _, stmt) ->
            get_unbounded_guesses stmt
            
        | StmtIf(_, stmt_true, stmt_false) ->
            (get_unbounded_guesses stmt_true) @ (get_unbounded_guesses stmt_false)
            
        | StmtGuess(id, stmt) ->
            id :: (get_unbounded_guesses stmt)
            
        | StmtGuessLowerBound(id, _, stmt) ->
            id :: (get_unbounded_guesses stmt)
            
        | StmtGuessLowerUpperBound(_, _, _, stmt) ->
            get_unbounded_guesses stmt
            
        | StmtAccept ->
            []
            
        | StmtReject ->
            []
    
    






(* ********** Evaluation ********** *)


let rec eval_expr env expr =
    match expr with
        ExprNumber(n) ->
            n
            
        | ExprVariable(id) ->
            List.assoc id env
            
        | ExprProduct(expr1, expr2) ->
            let val1 = eval_expr env expr1 in
            let val2 = eval_expr env expr2 in
            val1 * val2
            
        | ExprDivision(expr1, expr2) ->
            let val1 = eval_expr env expr1 in
            let val2 = eval_expr env expr2 in
            val1 / val2
            
        | ExprSum(expr1, expr2) ->
            let val1 = eval_expr env expr1 in
            let val2 = eval_expr env expr2 in
            val1 + val2
            
        | ExprSubtraction(expr1, expr2) ->
            let val1 = eval_expr env expr1 in
            let val2 = eval_expr env expr2 in
            val1 - val2
            
        | ExprPower(expr1, expr2) ->
            let val1 = eval_expr env expr1 in
            let val2 = eval_expr env expr2 in
            power val1 val2


let rec eval_cnd env cnd =
    match cnd with
        CndTrue ->
            true
        
        | CndFalse ->
            false

        | CndEqual(expr1, expr2) ->
            let val1 = eval_expr env expr1 in
            let val2 = eval_expr env expr2 in
            (=) val1 val2
            
        | CndLessthan(expr1, expr2) ->
            let val1 = eval_expr env expr1 in
            let val2 = eval_expr env expr2 in
            (<) val1 val2
            
        | CndNegation(cnd) ->
            let val1 = eval_cnd env cnd in
            (not) val1
            
        | CndOr(cnd1, cnd2) ->
            let val1 = eval_cnd env cnd1 in
            let val2 = eval_cnd env cnd2 in
            (||) val1 val2
        
        | CndAnd(cnd1, cnd2) ->
            let val1 = eval_cnd env cnd1 in
            let val2 = eval_cnd env cnd2 in
            (&&) val1 val2


let rec eval_stmt env guesses stmt =
    match stmt with 
        StmtLet(id, expr, stmt) ->
            let expr_value = eval_expr env expr in
            let env' = (id, expr_value) :: env in
            eval_stmt env' guesses stmt
        
        | StmtIf(cnd, stmt_true, stmt_false) ->
            let cnd_value = eval_cnd env cnd in
            eval_stmt env guesses (if cnd_value then stmt_true else stmt_false)
        
        | StmtGuess(id, stmt) ->
            let guess = List.assoc id guesses in
            let env' = (id, guess) :: env in
            eval_stmt env' guesses stmt
            
        | StmtGuessLowerBound(id, lower_bound_expr, stmt) ->
            let lower_bound = eval_expr env lower_bound_expr in
            let guess = List.assoc id guesses in
            if guess < lower_bound then 
                None
            else
                let env' = (id, guess) :: env in
                eval_stmt env' guesses stmt
            
        | StmtGuessLowerUpperBound(id, lower_bound_expr, upper_bound_expr, stmt) ->
            let lower_bound = eval_expr env lower_bound_expr in
            let upper_bound = eval_expr env upper_bound_expr in
            let rec helper guess =
                let env' = (id, guess) :: env in
                let result = eval_stmt env' guesses stmt in
                match result with
                    None ->
                        if guess < upper_bound then
                            helper (guess + 1)
                        else 
                            None
                    | Some(_) ->
                        result
            in
            helper lower_bound
        
        | StmtAccept ->
            Some(env)
            
        | StmtReject ->
            None


let eval stmt =
    let unbounded_guesses = get_unbounded_guesses stmt in
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
    