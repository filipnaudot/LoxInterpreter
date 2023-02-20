module Parser (parse) where

import qualified Data.Map as Map
import Data.Maybe
import Data.Char
import Debug.Trace      -- For debug prints.

import Scanner          -- Import the scanner.
import Tokens           -- Import data types.



-- Datatype for representing a Lox program
data Program = Program [Declaration] deriving (Show)

-- Datatype for representing a Lox declaration
data Declaration = VariableDecl Literal (Maybe Expr) | Statement Stmt deriving (Show)

-- Datatype for representing a Lox statement
data Stmt = ExprStmt Expr
           | PrintStmt Expr
           | BlockStmt [Declaration]
           | IfStmt Expr Stmt (Maybe Stmt)
           | WhileStmt Expr Stmt
           | ForStmt (Maybe Stmt) (Maybe Expr) (Maybe Expr) Stmt
           | ReturnStmt (Maybe Expr)
           | BreakStmt
           | ContinueStmt
           deriving (Show)

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
    deriving (Show)


---------------------------------------------------------
------------------- Helper functions --------------------
---------------------------------------------------------
-- TODO: Add getLine function

---------------------------------------------------------
------------------------ Main ---------------------------
---------------------------------------------------------

-- Parse a Lox program from a list of tokens
parse :: [Token] -> [Declaration]
parse (token:tokens) = 
  let (statement, rest) = buildDecl (token:tokens)
  in case rest of
    (TOKEN EOF _ _ _) : rest -> [statement]
    _ -> error "-----------------  Parse error  -----------------"




---------------------------------------------------------
--------------------- Declaration -----------------------
---------------------------------------------------------
-- Parse a Lox declaration from a list of tokens
buildDecl :: [Token] -> (Declaration, [Token])
buildDecl (token:tokens) = 
  case token of
    (TOKEN VAR _ _ _) -> buildVariableDecl (tokens)
    _ -> let (stmt, rest') = buildStatement (token:tokens)
         in (Statement stmt, rest')



---------------------------------------------------------
----------------- Variable declaration ------------------
---------------------------------------------------------
-- Parse a Lox variable declaration from a list of tokens
buildVariableDecl :: [Token] -> (Declaration, [Token])
buildVariableDecl toks@(TOKEN IDENTIFIER _ (ID idStr) _ : tokens) =
  case tokens of
    TOKEN SEMICOLON _ _ _ : rest1 -> (VariableDecl (ID idStr) Nothing, rest1)
    TOKEN EQUAL _ _ _ : exprTokens -> let (expr, rest) = buildExpr exprTokens
                                      in case rest of
                                        TOKEN SEMICOLON _ _ _ : rest2 -> (VariableDecl (ID idStr) (Just expr), rest2)
                                        _ -> error "Expected semicolon after variable declaration"
    _ -> error "Expected semicolon or equals sign after variable identifier"
buildVariableDecl _ = error "Expected 'var' keyword followed by identifier"



---------------------------------------------------------
----------------------- Statement -----------------------
---------------------------------------------------------
-- Parse a Lox statement from a list of tokens
buildStatement :: [Token] -> (Stmt, [Token])
buildStatement (token:tokens) = 
  case token of
    (TOKEN PRINT _ _ _) -> let (printStatement, rest) = buildPrintStatement (token:tokens)
                           in (printStatement, rest)
    _ -> let (exprStmt, rest') = buildExpr (token:tokens)
         in case rest' of
            TOKEN SEMICOLON _ _ _ : rest'' -> (ExprStmt exprStmt, rest'')
            _ -> error "Expected semicolon after expression statement"



---------------------------------------------------------
-------------------- Print statement --------------------
---------------------------------------------------------
-- Parse a Lox print statement from a list of tokens
buildPrintStatement :: [Token] -> (Stmt, [Token])
buildPrintStatement (token:tokens) = 
  let (printStmt, rest) = buildExpr tokens
  in case rest of
    TOKEN SEMICOLON _ _ _ : rest' -> (PrintStmt printStmt, rest')
    _ -> error "Expected semicolon after print statement"



---------------------------------------------------------
-------------------- Block statement --------------------
---------------------------------------------------------
-- Parse a Lox block statement from a list of tokens













---------------------------------------------------------
---------------------- Expression -----------------------
---------------------------------------------------------
buildExpr :: [Token] -> (Expr, [Token])
buildExpr tokens = buildAssignment tokens



buildAssignment :: [Token] -> (Expr, [Token])
buildAssignment (token:tokens) =
  case token of
    TOKEN IDENTIFIER strValue _ _ ->
      -- check for assignment
      case tokens of
        (TOKEN EQUAL _ _ _) : restTokens1 ->
          let (valueExpr, restTokens2) = buildAssignment restTokens1
          in (Assignment strValue valueExpr, restTokens2)
        _ -> buildLogicalOr (token:tokens)
    _ -> buildLogicalOr (token:tokens)



buildLogicalOr :: [Token] -> (Expr, [Token])
buildLogicalOr tokens =
  let (leftExpr, restTokens) = buildLogicalAnd tokens
  in case restTokens of
    (TOKEN OR _ _ _) : restTokens1 -> let (rightExpr, restTokens2) = buildLogicalOr restTokens1
                                      in (LogicalOr leftExpr rightExpr, restTokens2)
    _ -> (leftExpr, restTokens)
       


buildLogicalAnd :: [Token] -> (Expr, [Token])
buildLogicalAnd tokens = 
  let (left, restTokens) = buildEquality tokens
  in case restTokens of
    (TOKEN AND _ _ _) : restTokens1 -> 
      let (right, restTokens2) = buildLogicalAnd restTokens1
      in (LogicalAnd left right, restTokens2)
    _ -> (left, restTokens)



buildEquality :: [Token] -> (Expr, [Token])
buildEquality tokens =
  let (leftExpr, restTokens) = buildComparison tokens
  in case restTokens of
       (TOKEN BANG_EQUAL strBangEqual _ _) : restTokens1 -> let (rightExpr, restTokens2) = buildComparison restTokens1
                                                            in (Equality leftExpr strBangEqual rightExpr, restTokens2)
       (TOKEN EQUAL_EQUAL strEqualEqual _ _) : restTokens1 -> let (rightExpr, restTokens2) = buildComparison restTokens1
                                                              in (Equality leftExpr strEqualEqual rightExpr, restTokens2)
       _ -> (leftExpr, restTokens)



buildComparison :: [Token] -> (Expr, [Token])
buildComparison tokens =
  let (leftExpr, restTokens) = buildTerm tokens
  
  in case restTokens of
       -- > Term 
       (TOKEN GREATER strGreater _ _) : restTokens1 -> let (rightExpr, restTokens2) = buildTerm restTokens1
                                                       in (Term leftExpr strGreater rightExpr, restTokens2)
       -- >= Term 
       (TOKEN GREATER_EQUAL strGreaterEqual _ _) : restTokens1 -> let (rightExpr, restTokens2) = buildTerm restTokens1
                                                                  in (Term leftExpr strGreaterEqual rightExpr, restTokens2)
       -- < Term 
       (TOKEN LESS strLess _ _) : restTokens1 -> let (rightExpr, restTokens2) = buildTerm restTokens1
                                                 in (Term leftExpr strLess rightExpr, restTokens2)
       -- <= Term 
       (TOKEN LESS_EQUAL strLessEqual _ _) : restTokens1 -> let (rightExpr, restTokens2) = buildTerm restTokens1
                                                            in (Term leftExpr strLessEqual rightExpr, restTokens2)
       _ -> (leftExpr, restTokens)
                                          


buildTerm :: [Token] -> (Expr, [Token])
buildTerm tokens =
  let (leftExpr, restTokens) = buildFactor tokens
  in case restTokens of
       (TOKEN MINUS strMinus _ _) : restTokens1 -> let (rightExpr, restTokens2) = buildFactor restTokens1
                                                   in (Term leftExpr strMinus rightExpr, restTokens2)
       (TOKEN PLUS strPlus _ _) : restTokens1 -> let (rightExpr, restTokens2) = buildFactor restTokens1
                                                 in (Term leftExpr strPlus rightExpr, restTokens2)
       _ -> (leftExpr, restTokens)



buildFactor :: [Token] -> (Expr, [Token])
buildFactor tokens =
  let (leftExpr, restTokens) = buildUnary tokens
  in case restTokens of
       (TOKEN SLASH strSlash _ _) : restTokens1 -> let (rightExpr, restTokens2) = buildUnary restTokens1
                                                 in (Unary strSlash rightExpr, restTokens2)
       (TOKEN STAR strStar _ _) : restTokens1 -> let (rightExpr, restTokens2) = buildUnary restTokens1
                                               in (Unary strStar rightExpr, restTokens2)
       _ -> (leftExpr, restTokens)



buildUnary :: [Token] -> (Expr, [Token])
buildUnary tokens =
  case tokens of
    (TOKEN BANG strBang _ _) : restTokens1 -> let (unaryExpr, restTokens2) = buildUnary restTokens1
                                            in (Unary strBang unaryExpr, restTokens2)
    (TOKEN MINUS strMinus _ _) : restTokens1 -> let (unaryExpr, restTokens2) = buildUnary restTokens1
                                              in (Unary strMinus unaryExpr, restTokens2)
    _ -> buildPrimary tokens



buildPrimary :: [Token] -> (Expr, [Token])
buildPrimary ((TOKEN TRUE _ TRUE_LIT _) : tokens) = (Primary TRUE_LIT, tokens)
buildPrimary ((TOKEN FALSE _ FALSE_LIT _) : tokens) = (Primary FALSE_LIT, tokens)
buildPrimary ((TOKEN NIL _ NIL_LIT _) : tokens) = (Primary NIL_LIT, tokens)
buildPrimary ((TOKEN NUMBER _ (NUM num) _) : tokens) = (Primary (NUM num), tokens)
buildPrimary ((TOKEN STRING _ (STR str) _) : tokens) = (Primary (STR str), tokens)
buildPrimary ((TOKEN IDENTIFIER _ (ID str) _) : tokens) = (Primary (ID str), tokens)
buildPrimary ((TOKEN LEFT_PAREN _ _ _) : tokens) =
  let (expr, tokenRest1) = buildExpr tokens
  in case tokenRest1 of
       (TOKEN RIGHT_PAREN _ _ _) : tokenRest2 -> (Grouping expr, tokenRest2)
       _ -> error "Expected ')' after expression in grouping."
buildPrimary _ = error "Expected expression."

    
