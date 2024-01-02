import Text.Read (readEither)
data Expr = Lit Int
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr


eval :: Expr -> Int
eval ex =
    case ex of
        Lit x -> x
        Add ex1 ex2 -> (eval ex1) + (eval ex2)
        Sub ex1 ex2 -> (eval ex1) - (eval ex2)
        Mul ex1 ex2 -> (eval ex1) * (eval ex2)
        Div ex1 ex2 -> (eval ex1) `div` (eval ex2)


parse :: String -> Either String Expr
parse str =
    case parse' (words str) of
        Left err -> Left err
        Right (e, []) -> Right e
        Right (_, rest) -> Left $ "Found extra tokens "<> (unwords rest)

parse' :: [String] -> Either String (Expr, [String])
parse' [] = Left "Unexpected end of expression"
parse' (token:rest) =
    case token of
        "+" -> parseBinary Add rest
        "-" -> parseBinary Sub rest
        "*" -> parseBinary Mul rest
        "/" -> parseBinary Div rest
        lit ->
            case readEither lit of
                Left err -> Left err
                Right lit' -> Right (Lit lit', rest)

parseBinary :: (Expr -> Expr -> Expr) -> [String] -> Either String (Expr, [String])
parseBinary exprConstructor args =
    case parse' args of
        Left err -> Left err
        Right (firstArg, rest') ->
            case parse' rest' of
                Left err -> Left err
                Right (secondArg, rest'') ->
                    Right $ (exprConstructor firstArg secondArg, rest'')

run :: String -> String
run expr =
    case parse expr of
        Left err -> "Error: "<> err
        Right expr' ->
                let answer = show $ eval expr'
                in "the answer is "<> answer