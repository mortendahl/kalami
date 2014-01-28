
exception Error of string

type identifier = string

type number = int

type bound = int
    
type expression =
    ExprNumber of number
    | ExprVariable of identifier
    | ExprProduct of expression * expression
    | ExprDivision of expression * expression
    | ExprSum of expression * expression
    | ExprSubtraction of expression * expression
    | ExprPower of expression * expression
    
type condition =
    CndTrue
    | CndFalse
    | CndEqual of expression * expression
    | CndLessthan of expression * expression
    | CndNegation of condition
    | CndOr of condition * condition
    | CndAnd of condition * condition
    
type statement =
    StmtLet of identifier * expression * statement
    | StmtIf of condition * statement * statement
    | StmtGuess of identifier * statement
    | StmtGuessLowerBound of identifier * expression * statement
    | StmtGuessLowerUpperBound of identifier * expression * expression * statement
    | StmtAccept
    | StmtReject
