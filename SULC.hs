import Data.Char
import Data.List
import Control.Monad

data Term
  = Var String
  | Lam String Term
  | App Term Term
  deriving (Eq)

instance Show Term where
  show (Var x)     = x
  show (Lam x t)   = "λ" ++ x ++ ". " ++ show t
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

freeVars :: Term -> [String]
freeVars (Var x) = [x]
freeVars (Lam x t) = freeVars t \\ [x]
freeVars (App t1 t2) = nub (freeVars t1 ++ freeVars t2)

subst :: String -> Term -> Term -> Term
subst x s (Var y)
  | x == y    = s
  | otherwise = Var y
subst x s (Lam y t1)
  | x == y    = Lam y t1  
  | y `elem` freeVars s = 
      let y' = freshVar y [s, t1] in
      Lam y' (subst x s (subst y (Var y') t1))
  | otherwise = Lam y (subst x s t1)
subst x s (App t1 t2) = App (subst x s t1) (subst x s t2)

freshVar :: String -> [Term] -> String
freshVar x ts = head $ filter (`notElem` used) candidates
  where
    used = concatMap freeVars ts
    candidates = x : [x ++ show n | n <- [(1::Int)..]]

beta :: Term -> Maybe Term
beta (App (Lam x t1) t2) = Just $ subst x t2 t1
beta (App t1 t2) = 
  case beta t1 of
    Just t1' -> Just $ App t1' t2
    Nothing  -> App t1 <$> beta t2
beta (Lam x t) = Lam x <$> beta t
beta (Var _) = Nothing

eval :: Term -> Term
eval = evalWithLimit 1000
-- step limit

evalWithLimit :: Int -> Term -> Term
evalWithLimit 0 t = t  
evalWithLimit n t = 
  case beta t of
    Just t' -> evalWithLimit (n-1) t'
    Nothing -> t


main :: IO ()
main = do
  putStrLn "Lambda Calculus REPL"
  putStrLn "Syntax: \\x. body  or  λx. body  for lambda abstraction"
  putStrLn "Example: \\x. x  (identity function)"
  putStrLn "Example: (\\x. x) y  (application)"
  putStrLn "Type :quit to exit"
  repl

repl :: IO ()
repl = do
  putStr "λ> "
  line <- getLine
  if line == ":quit"
    then putStrLn "Goodbye!"
    else do
      case parseTerm line of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right term -> do
          putStrLn $ "Parsed: " ++ show term
          let result = eval term
          putStrLn $ "Result: " ++ show result
      repl

parseTerm :: String -> Either String Term
parseTerm input = 
  case runParser parseExpression (tokenize input) of
    Right (term, []) -> Right term
    Right (_, remaining) -> Left $ "Unexpected tokens: " ++ unwords remaining
    Left err -> Left err

tokenize :: String -> [String]
tokenize [] = []
tokenize (c:cs)
  | isSpace c = tokenize cs
  | c `elem` "().\\" = [c] : tokenize cs
  | c == 'λ' = "\\" : tokenize cs  
  | isAlphaNum c || c == '_' = 
      let (var, rest) = span (\x -> isAlphaNum x || x == '_') (c:cs)
      in var : tokenize rest
  | otherwise = tokenize cs  

type Parser = [String] -> Either String (Term, [String])

runParser :: Parser -> [String] -> Either String (Term, [String])
runParser p tokens = p tokens
parseExpression :: Parser
parseExpression tokens = do
  (term, rest) <- parseApplication tokens
  return (term, rest)

parseApplication :: Parser
parseApplication tokens = do
  (first, rest1) <- parseLambdaOrPrimary tokens
  parseApplicationRest first rest1

parseApplicationRest :: Term -> Parser
parseApplicationRest acc [] = Right (acc, [])
parseApplicationRest acc tokens =
  case parsePrimary tokens of
    Right (term, rest) -> parseApplicationRest (App acc term) rest
    Left _ -> Right (acc, tokens)

parseLambdaOrPrimary :: Parser
parseLambdaOrPrimary ("\\":var:".":rest) 
  | isValidVar var = do
      (body, remaining) <- parseExpression rest
      return (Lam var body, remaining)
  | otherwise = Left $ "Invalid variable name: " ++ var
parseLambdaOrPrimary tokens = parsePrimary tokens

parsePrimary :: Parser
parsePrimary [] = Left "Unexpected end of input"
parsePrimary ("(":tokens) = do
  (expr, rest1) <- parseExpression tokens
  case rest1 of
    (")":rest2) -> Right (expr, rest2)
    _ -> Left "Missing closing parenthesis"
parsePrimary (var:rest)
  | isValidVar var = Right (Var var, rest)
  | otherwise = Left $ "Invalid token: " ++ var

isValidVar :: String -> Bool
isValidVar [] = False
isValidVar (c:cs) = (isAlpha c || c == '_') && all (\x -> isAlphaNum x || x == '_') cs