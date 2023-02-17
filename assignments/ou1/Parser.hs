module Parser (buildExpr) where

import qualified Data.Map as Map
import Data.Maybe
import Data.Char
import Debug.Trace      -- For debug prints.

import Scanner          -- Import the scanner.
import Tokens           -- Import data types.



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
    deriving (Show)




buildExpr :: [Token] -> (Expr, [Token])
buildExpr tokens = buildAssignment tokens



buildAssignment :: [Token] -> (Expr, [Token])
buildAssignment (token:tokens) =
  case token of
    TOKEN IDENTIFIER strValue _ _ ->  -- check for an assignment
      case tokens of
        (TOKEN EQUAL _ _ _) : restTokens1 -> let (valueExpr, restTokens2) = buildAssignment restTokens1
                                              in (Assignment strValue valueExpr, restTokens2)

    _ -> buildLogicalOr (token : tokens)
  

{-
buildLogicalOr :: [Token] -> (Expr, [Token])
buildLogicalOr tokens =



buildLogicalAnd :: [Token] -> (Expr, [Token])
buildLogicalAnd tokens =



buildEquality :: [Token] -> (Expr, [Token])
buildEquality tokens =



buildComparison :: [Token] -> (Expr, [Token])
buildComparison tokens =


buildTerm :: [Token] -> (Expr, [Token])
buildTerm tokens =


buildFactor :: [Token] -> (Expr, [Token])
build tokens =



buildFactor :: [Token] -> (Expr, [Token])
build tokens =



buildUnary :: [Token] -> (Expr, [Token])
buildUnary tokens =



buildPrimary :: [Token] -> (Expr, [Token])
buildPrimary tokens =
-}