
open Structure

let printlist printfun eles seperator =
    let rec helper printfun eles prefix =
        match eles with
            [] -> ()
            | ele::rest ->
                print_string prefix;
                printfun ele;
                helper printfun rest seperator
    in
    helper printfun eles ""
    
let print_env env =
    printlist 
        (fun (id, value) -> print_string id; print_string " = "; print_int value)
        env
        "\n";
    print_string "\n"
    
let print_id id = print_string id

let print_bound bound = print_int bound
    
let rec print_expr expr =
    match expr with
        ExprNumber(n) ->
            print_int n
            
        | ExprVariable(id) ->
            print_string id
            
        | ExprProduct(expr1, expr2) ->
            print_string "(";
            print_expr expr1;
            print_string ")*(";
            print_expr expr2;
            print_string ")"
            
        | ExprDivision(expr1, expr2) ->
            print_string "(";
            print_expr expr1;
            print_string ")/(";
            print_expr expr2;
            print_string ")"
            
        | ExprSum(expr1, expr2) ->
            print_string "(";
            print_expr expr1;
            print_string ")+(";
            print_expr expr2;
            print_string ")"
            
        | ExprSubtraction(expr1, expr2) ->
            print_string "(";
            print_expr expr1;
            print_string ")-(";
            print_expr expr2;
            print_string ")"
            
        | ExprPower(expr1, expr2) ->
            print_string "(";
            print_expr expr1;
            print_string ")**(";
            print_expr expr2;
            print_string ")"

let rec print_cnd cnd =
    match cnd with 
        CndTrue ->
            print_string "true"

        | CndFalse ->
            print_string "false"
            
        | CndEqual(expr1, expr2) ->
            print_expr expr1;
            print_string "==";
            print_expr expr2
            
        | CndLessthan(expr1, expr2) ->
            print_expr expr1;
            print_string "<";
            print_expr expr2
            
        | CndNegation(cnd) ->
            print_string "not ";
            print_cnd cnd
            
        | CndOr(cnd1, cnd2) ->
            print_cnd cnd1;
            print_string " or ";
            print_cnd cnd2
            
        | CndAnd(cnd1, cnd2) ->
            print_cnd cnd1;
            print_string " and ";
            print_cnd cnd2
    
let print_stmt stmt = 
    let rec helper indent stmt = 
        match stmt with
            StmtLet(id, expr, stmt) ->
                print_string indent;
                print_string "let ";
                print_id id;
                print_string " = ";
                print_expr expr;
                print_string " in \n";
                helper indent stmt
                
            | StmtIf(cnd, stmt_true, stmt_false) ->
                print_string indent;
                print_string "if ";
                print_cnd cnd;
                print_string " then \n";
                helper (indent^"  ") stmt_true;
                print_string ("\n" ^ indent);
                print_string "otherwise \n";
                helper (indent^"  ") stmt_false
                
            | StmtGuess(id, stmt) ->
                print_string indent;
                print_string "guess ";
                print_id id;
                print_string " in \n";
                helper indent stmt
                
            | StmtGuessLowerBound(id, lower_bound, stmt) ->
                print_string indent;
                print_string "guess ";
                print_id id;
                print_string " from ";
                print_expr lower_bound;
                print_string " in \n";
                helper indent stmt
                
            | StmtGuessLowerUpperBound(id, lower_bound, upper_bound, stmt) ->
                print_string indent;
                print_string "guess ";
                print_id id;
                print_string " from ";
                print_expr lower_bound;
                print_string " to ";
                print_expr upper_bound;
                print_string " in \n";
                helper indent stmt
                
            | StmtAccept ->
                print_string indent;
                print_string "accept"
                
            | StmtReject ->
                print_string indent;
                print_string "reject"
    in
    helper "" stmt
