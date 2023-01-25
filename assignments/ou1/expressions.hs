import Data.Char

data Expr = Num Int
           | Add Expr Expr
           | Sub Expr Expr
           | Mult Expr Expr
           deriving (Show)


eval :: Expr -> Int
eval (Num n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mult e1 e2) = (eval e1) * (eval e2)


calc :: [Char] -> Int
calc str = let
                (tree, rest) = buildExpr str
           in
                case rest of
                   [] -> eval tree
                   _  -> error "Expression not syntactically correct"


buildExpr :: [Char] -> (Expr, [Char])
buildExpr str = let
                    (term, rest) = buildTerm str
                in
                    case rest of
                      '+':rest2 -> let
                                      (factor, rest3) = buildFactor rest2
                                   in
                                      (Add term factor, rest3)
                      '-':rest2 -> let
                                      (factor, rest3) = buildFactor rest2
                                   in
                                      (Sub term factor, rest3)
                      _         -> (term,rest)


buildTerm :: [Char] -> (Expr, [Char])
buildTerm str = let
                     (factor, rest) = buildFactor str
                in
                     case rest of
                        '*':rest2 -> let
                                        (term, rest3) = buildTerm rest2
                                     in
                                        (Mult factor term, rest3)
                        _         -> (factor, rest)


buildFactor :: [Char] -> (Expr, [Char])
buildFactor (c:str) 
    | isDigit c = let
                        (number, rest) = buildNum (ord c - ord '0') str
                  in
                        (Num number, rest)
    | c == '('  = let
                        (expr, rest) = buildExpr str
                  in
                        case rest of
                            ')':rest2 -> (expr, rest2)
                            _         -> error "Expected ')'"


buildNum :: Int -> [Char] -> (Int, [Char])
buildNum currentVal [] = (currentVal, [])
buildNum currentVal str@(c:rest)
    | isDigit c = buildNum (currentVal*10 + (ord c - ord '0')) rest
    | otherwise = (currentVal, str)
                                             