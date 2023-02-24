module LoxGrammar where

import Tokens




---------------------------------------------------------
---------------- Lox grammar/data types -----------------
---------------------------------------------------------

-- Datatype for representing a Lox program
data Program = PROGRAM [Declaration]

instance Show Program where
  show (PROGRAM decls) = show (length decls) ++ "\n" ++ unlines (map show decls)



-- Datatype for representing a Lox declaration
data Declaration = VariableDecl Literal (Maybe Expr) | Statement Stmt

instance Show Declaration where
  show (VariableDecl id Nothing) = "V DEC -> " ++ getLiteral id ++ ";"
  show (VariableDecl id (Just expr)) = "V DEC -> " ++ getLiteral id ++ "=" ++ show expr ++ ";"
  show (Statement stmt) = show stmt



-- Datatype for representing a Lox statement
data Stmt = ExprStmt Expr
           | PrintStmt Expr
           | BlockStmt [Declaration]
           | IfStmt Expr Stmt (Maybe Stmt)
           | WhileStmt Expr Stmt
           | ForStmt (Maybe Stmt) (Maybe Expr) (Maybe Expr) Stmt
           | ReturnStmt (Maybe Expr)

instance Show Stmt where
  show (ExprStmt expr) = show expr ++ ";"
  show (PrintStmt expr) = "print" ++ show expr ++ ";"
  show (BlockStmt decls) = "{" ++ unwords (map show decls) ++ "}"
  show (IfStmt condition stmt Nothing) = "if(" ++ show condition ++ ")" ++ show stmt
  show (IfStmt condition stmt (Just elseStmt)) = "if(" ++ show condition ++ ")" ++ show stmt ++ "else" ++ show elseStmt
  show (WhileStmt condition stmt) = "while(" ++ show condition ++ ")" ++ show stmt
  show (ForStmt initializer condition increment stmt) = "for(" ++ show initializer ++ ";" ++ show condition ++ ";" ++ show increment ++ ")" ++ show stmt
  show (ReturnStmt (Just expr)) = "return" ++ show expr ++ ";"
  show (ReturnStmt Nothing) = "return;"



-- Datatype for representing a Lox expression
data Expr
    = Assignment String Expr       -- variable name, value to assign
    | LogicalOr Expr Expr          -- left operand, right operands
    | LogicalAnd Expr Expr         -- left operand, right operands
    | Equality Expr String Expr    -- left operand, operator, right operand
    | Comparison Expr String Expr  -- left operand, operator, right operand
    | Term Expr String Expr        -- left operand, operator, right operand
    | Factor Expr String Expr      -- left operand, operator, right operand
    | Unary String Expr            -- operator, operand
    | Primary Literal              -- a literal
    | Grouping Expr

instance Show Expr where
    show (Assignment id expr) = id ++ "=" ++ show expr
    show (LogicalOr expr1 expr2) = "(" ++ show expr1 ++ "||" ++ show expr2 ++ ")"
    show (LogicalAnd expr1 expr2) = "(" ++ show expr1 ++ "&&" ++ show expr2  ++ ")"
    show (Equality expr1 operator expr2) = "(" ++ show expr1 ++ operator ++ show expr2 ++ ")"
    show (Comparison expr1 operator expr2) = show expr1 ++ operator ++ show expr2
    show (Term expr1 operator expr2) = "(" ++ show expr1 ++ operator ++ show expr2 ++ ")"
    show (Factor expr1 operator expr2) = "(" ++ show expr1 ++ operator ++ show expr2 ++ ")"
    show (Unary operator expr) = "(" ++ operator ++ show expr ++ ")"
    show (Primary lit) = getLiteral lit
    show (Grouping expr) = "(" ++ show expr ++ ")"



---------------------------------------------------------
------------------- Helper functions --------------------
---------------------------------------------------------

-- Format/retrieve the actual value of a literal for show function.
-- Needed because literal has deriving show.
getLiteral (STR str) = "\"" ++ str ++ "\""
getLiteral (ID id) = id
getLiteral (NUM num) = show num
getLiteral literal = show literal