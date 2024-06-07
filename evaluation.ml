
open Structure
open Printing


    

(* ********** Evaluation ********** *)


(* TODO: repeated squaring, I am crypto for fucks' sake! *)
let rec power base exponent =
    if (=) exponent 0 then 1
    else base * (power base (exponent - 1))


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
    (*let rec guess_helper_upperbound id lower_bound upper_bound stmt =
        let guess = lower_bound in
        let env' = (id, guess) :: env in
        let result = eval_stmt env' guesses stmt in
        match result with
            None ->
                if guess < upper_bound then
                    guess_helper_upperbound id (guess + 1) upper_bound stmt
                else 
                    None
            | Some(_) ->
                result
    in*)
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
