module Parser (parse) where

import qualified Data.Map as Map
import Data.Maybe
import Data.Char
import Debug.Trace      -- For debug prints.

import Scanner          -- Import the scanner.
import Tokens           -- Import data types.
import LoxGrammar


--
-- Lox parser.
--
-- Author: Filip Naudot (ens19fpt@cs.umu.se)
--
-- Version information:
--   2023-02-23: v1.0
--
---------------------------------------------------------
------------------------ Main ---------------------------
---------------------------------------------------------
-- Parse a Lox program from a list of tokens
parse :: [Token] -> Program
parse tokens = let (decls, []) = buildDecls tokens
               in PROGRAM decls


---------------------------------------------------------
--------------------- Declaration -----------------------
---------------------------------------------------------
-- Parse a list of Lox declarations from a list of tokens
buildDecls :: [Token] -> ([Declaration], [Token])
buildDecls (TOKEN EOF _ _ _ : []) = ([], [])
buildDecls (TOKEN EOF _ _ line : _) = error $ "Unexpected EOF token on line " ++ show line ++ ". EOF token should be the last token."
buildDecls toks@(TOKEN RIGHT_BRACE _ _ _ : rest) = ([], toks)
buildDecls tokens = 
  let (decl, rest) = buildDecl tokens
      (decls, tokens') = buildDecls rest
  in (decl : decls, tokens')


-- Parse a Lox declaration from a list of tokens
buildDecl :: [Token] -> (Declaration, [Token])
buildDecl (token:tokens) = 
  case token of
    (TOKEN VAR _ _ _) -> buildVariableDecl tokens
    _ -> let (stmt, rest') = buildStatement (token:tokens)
         in (Statement stmt, rest')


---------------------------------------------------------
----------------- Variable declaration ------------------
---------------------------------------------------------
-- Parse a Lox variable declaration from a list of tokens
buildVariableDecl :: [Token] -> (Declaration, [Token])
buildVariableDecl toks@(TOKEN IDENTIFIER _ (ID idStr) line : tokens) =
  case tokens of
    TOKEN SEMICOLON _ _ _ : rest1 -> (VariableDecl (ID idStr) Nothing, rest1)
    TOKEN EQUAL _ _ line : exprTokens -> let (expr, rest) = buildExpr exprTokens
                                      in case rest of
                                        TOKEN SEMICOLON _ _ _ : rest2 -> (VariableDecl (ID idStr) (Just expr), rest2)
                                        _ -> error ("Expected semicolon after variable declaration on line " ++ show line)
    _ -> error ("Expected semicolon or equals sign after variable identifier on line " ++ show line)
buildVariableDecl _ = error "Expected 'var' keyword followed by identifier"


---------------------------------------------------------
----------------------- Statement -----------------------
---------------------------------------------------------
-- Parse a Lox statement from a list of tokens
buildStatement :: [Token] -> (Stmt, [Token])
buildStatement (token:tokens) = 
  --trace ("buildStatement: " ++ show (token:tokens)) $ -- trace input tokens list
  case token of
    (TOKEN IF _ _ _) -> buildIfStatement (tokens)
    (TOKEN PRINT _ _ _) -> buildPrintStatement (token:tokens)
    (TOKEN RETURN _ _ _) -> buildReturnStatement (tokens)
    (TOKEN WHILE _ _ _) -> buildWhileStatement (tokens)
    (TOKEN LEFT_BRACE _ _ _) -> buildBlockStatement (tokens)
    _ -> let (exprStmt, rest') = buildExpr (token:tokens)
         in case rest' of
            TOKEN SEMICOLON _ _ _ : rest'' -> (ExprStmt exprStmt, rest'')
            _ -> error "Expected semicolon after expression statement"


---------------------------------------------------------
---------------------- if statement ---------------------
---------------------------------------------------------
-- Parse a Lox if statement from a list of tokens
buildIfStatement :: [Token] -> (Stmt, [Token])
buildIfStatement toks@(TOKEN LEFT_PAREN _ _ _ : tokens) =
  let (exprStmt, rest) = buildExpr tokens
  in case rest of
    TOKEN RIGHT_PAREN _ _ _ : rest' -> let (ifStmt, rest'') = buildStatement rest'
                                       in case rest'' of
                                        (TOKEN ELSE _ _ _) : rest3 -> 
                                          let (elseStmt, rest4) = buildStatement rest3
                                          in (IfStmt exprStmt ifStmt (Just elseStmt), rest4)
                                        _ -> (IfStmt exprStmt ifStmt Nothing, rest'')
    _ -> error "Expected ')' after if statement"
buildIfStatement _ = error "Expected 'if' keyword followed by '('"  


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
-------------------- Return statement -------------------
---------------------------------------------------------
buildReturnStatement :: [Token] -> (Stmt, [Token])
buildReturnStatement toks@(token:tokens) =
  let (maybeExpr, rest) =
        case token of
          (TOKEN SEMICOLON _ _ _) -> (Nothing, toks) -- return;
          _ -> let (expr, rest'') = buildExpr toks -- return <expression>;
               in (Just expr, rest'')
  in case rest of
    (TOKEN SEMICOLON _ _ _) : rest' -> (ReturnStmt maybeExpr, rest')
    _ -> error "Expected semicolon after return statement"


---------------------------------------------------------
-------------------- While statement -------------------
---------------------------------------------------------
-- Parse a Lox while statement from a list of tokens
buildWhileStatement :: [Token] -> (Stmt, [Token])
buildWhileStatement toks@(TOKEN LEFT_PAREN _ _ _ : tokens) =
  let (exprStmt, rest) = buildExpr tokens
  in case rest of
    TOKEN RIGHT_PAREN _ _ _ : rest' -> let (stmt, rest'') = buildStatement rest'
                                       in (WhileStmt exprStmt stmt, rest'')
    _ -> error "Expected ')'"
buildWhileStatement _ = error "Expected '('  after while statement"


---------------------------------------------------------
-------------------- Block statement --------------------
---------------------------------------------------------
-- Parse a Lox block statement from a list of tokens
buildBlockStatement :: [Token] -> (Stmt, [Token])
buildBlockStatement toks@(token:tokens) =
  let (subdecl, rest) = buildDecls toks
  in case rest of
    (TOKEN RIGHT_BRACE _ _ _) : rest' -> (BlockStmt subdecl, rest')
    _ -> error "expected '}' after block"






---------------------------------------------------------
---------------------- Expression -----------------------
---------------------------------------------------------
-- buildExpr  - Builds an antire expression statement.
--               
-- Input:
--  [Token] - List of lox-tokens
-- Returns a Expr containing the Expr value and the rest of the tokens.
buildExpr :: [Token] -> (Expr, [Token])
buildExpr tokens = buildAssignment tokens


-- buildAssignment  - Builds an assignment statement. Checks
--                    for identifier followed by equal sign.
--               
-- Input:
--  [Token] - List of lox-tokens
-- Returns a Expr containing the Expr value and the rest of the tokens.
buildAssignment :: [Token] -> (Expr, [Token])
buildAssignment (token:tokens) =
  case token of
    TOKEN IDENTIFIER strValue _ _ ->
      -- check for assignment
      case tokens of
        (TOKEN EQUAL _ _ _) : restTokens1 -> let (valueExpr, restTokens2) = buildAssignment restTokens1
                                             in (Assignment strValue valueExpr, restTokens2)
        _ -> buildLogicalOr (token:tokens)
    _ -> buildLogicalOr (token:tokens)


-- buildLogicalOr  - Builds logical or statement. Consists of 
--                   logical and statements on both sides.
--               
-- Input:
--  [Token] - List of lox-tokens
-- Returns a Expr containing the Expr value and the rest of the tokens.
buildLogicalOr :: [Token] -> (Expr, [Token])
buildLogicalOr tokens =
  let (leftExpr, restTokens) = buildLogicalAnd tokens
  in case restTokens of
    (TOKEN OR _ _ _) : restTokens1 -> let (rightExpr, restTokens2) = buildLogicalOr restTokens1
                                      in (LogicalOr leftExpr rightExpr, restTokens2)
    _ -> (leftExpr, restTokens)
       

-- buildLogicalAnd  - Builds logical and statement. Consists of 
--                    equality statements on both sides.
--               
-- Input:
--  [Token] - List of lox-tokens
-- Returns a Expr containing the Expr value and the rest of the tokens.
buildLogicalAnd :: [Token] -> (Expr, [Token])
buildLogicalAnd tokens = 
  let (left, restTokens) = buildEquality tokens
  in case restTokens of
    (TOKEN AND _ _ _) : restTokens1 -> 
      let (right, restTokens2) = buildLogicalAnd restTokens1
      in (LogicalAnd left right, restTokens2)
    _ -> (left, restTokens)


-- buildEquality  - Builds equality statement. Checks if it
--                  should evaluate equal or not equal.
--               
-- Input:
--  [Token] - List of lox-tokens
-- Returns a Expr containing the Expr value and the rest of the tokens.
buildEquality :: [Token] -> (Expr, [Token])
buildEquality tokens =
  let (leftExpr, restTokens) = buildComparison tokens
  in case restTokens of
    (TOKEN BANG_EQUAL strBangEqual _ _) : restTokens1 -> let (rightExpr, restTokens2) = buildComparison restTokens1
                                                         in (Equality leftExpr strBangEqual rightExpr, restTokens2)
    (TOKEN EQUAL_EQUAL strEqualEqual _ _) : restTokens1 -> let (rightExpr, restTokens2) = buildComparison restTokens1
                                                           in (Equality leftExpr strEqualEqual rightExpr, restTokens2)
    _ -> (leftExpr, restTokens)


-- buildComparison  - Builds comprison statement. A comprison can
--                    be one of the following >, >=, <, <=.
--               
-- Input:
--  [Token] - List of lox-tokens
-- Returns a Expr containing the Expr value and the rest of the tokens.
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
                                          

-- buildTerm  - Builds term. A term contains MINUS
--              for subtraction or PLUS for addition.
--               
-- Input:
--  [Token] - List of lox-tokens
-- Returns a Expr containing the Expr value and the rest of the tokens.
buildTerm :: [Token] -> (Expr, [Token])
buildTerm tokens =
  let (leftExpr, restTokens) = buildFactor tokens
  in case restTokens of
    (TOKEN MINUS strMinus _ _) : restTokens1 -> let (rightExpr, restTokens2) = buildTerm restTokens1
                                                in (Term leftExpr strMinus rightExpr, restTokens2)
    (TOKEN PLUS strPlus _ _) : restTokens1 -> let (rightExpr, restTokens2) = buildTerm restTokens1
                                              in (Term leftExpr strPlus rightExpr, restTokens2)
    _ -> (leftExpr, restTokens)


-- buildFactor  - Builds factor. A factor contains SLASH
--                for division or STAR for multiplication.
--               
-- Input:
--  [Token] - List of lox-tokens
-- Returns a Expr containing the Expr value and the rest of the tokens.
buildFactor :: [Token] -> (Expr, [Token])
buildFactor tokens =
  let (leftExpr, restTokens) = buildUnary tokens
  in case restTokens of
    (TOKEN SLASH strSlash _ _) : restTokens1 -> let (rightExpr, restTokens2) = buildFactor restTokens1
                                                in (Factor leftExpr strSlash rightExpr, restTokens2)
    (TOKEN STAR strStar _ _) : restTokens1 -> let (rightExpr, restTokens2) = buildFactor restTokens1
                                              in (Factor leftExpr strStar rightExpr, restTokens2)
    _ -> (leftExpr, restTokens)


-- buildUnary  - Builds unary. Check for negations and calls buildPrimary.
--               
-- Input:
--  [Token] - List of lox-tokens
-- Returns a Expr containing the Expr value and the rest of the tokens.
buildUnary :: [Token] -> (Expr, [Token])
buildUnary tokens =
  case tokens of
    (TOKEN BANG strBang _ _) : restTokens1 -> let (unaryExpr, restTokens2) = buildUnary restTokens1
                                              in (Unary strBang unaryExpr, restTokens2)
    (TOKEN MINUS strMinus _ _) : restTokens1 -> let (unaryExpr, restTokens2) = buildUnary restTokens1
                                                in (Unary strMinus unaryExpr, restTokens2)
    _ -> buildPrimary tokens


-- buildPrimary  - Builds primary. End point for expression.
--               
-- Input:
--  [Token] - List of lox-tokens
-- Returns a Expr containing the Expr value and the rest of the tokens.
buildPrimary :: [Token] -> (Expr, [Token])
buildPrimary ((TOKEN TRUE _ TRUE_LIT _) : tokens) = (Primary TRUE_LIT, tokens)
buildPrimary ((TOKEN FALSE _ FALSE_LIT _) : tokens) = (Primary FALSE_LIT, tokens)
buildPrimary ((TOKEN NIL _ NIL_LIT _) : tokens) = (Primary NIL_LIT, tokens)
buildPrimary ((TOKEN NUMBER _ (NUM num) _) : tokens) = (Primary (NUM num), tokens)
buildPrimary ((TOKEN STRING _ (STR str) _) : tokens) = (Primary (STR str), tokens)
buildPrimary ((TOKEN IDENTIFIER _ (ID str) _) : tokens) = (Primary (ID str), tokens)
buildPrimary ((TOKEN LEFT_PAREN _ _ line) : tokens) =
  let (expr, tokenRest1) = buildExpr tokens
  in case tokenRest1 of
       (TOKEN RIGHT_PAREN _ _ _) : tokenRest2 -> (Grouping expr, tokenRest2)
       _ -> error ("Expected ')' after expression grouping. Opening '(' is on line " ++ show line)
buildPrimary ((TOKEN _ _ _ line) : _) = error ("Expected expression on line " ++ show line)