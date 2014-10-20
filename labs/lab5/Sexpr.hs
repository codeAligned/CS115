--
-- S-expression parser.
--

module Sexpr where

import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------------
-- Datatypes.
----------------------------------------------------------------------

data Atom =
    BoolA  Bool
  | IntA   Integer
  | FloatA Double
  | IdA    String  -- identifier
  | StringA String
  deriving (Show)

data Sexpr =
    AtomS Atom
  | ListS [Sexpr]
  deriving (Show)

----------------------------------------------------------------------
-- Parsers.
----------------------------------------------------------------------

parseBool :: Parser Bool
parseBool =
  char '#' >>
  ((char 'f' >> return False)
   <|> (char 't' >> return True))
  <?> "boolean"

parseInt :: Parser Integer
parseInt = do
  sign <- option "" (string "-")
  digits <- many1 digit  -- many1 (oneOf "0123456789")
  return (read (sign ++ digits) :: Integer)
  <?> "integer"

parseFloat :: Parser Double
parseFloat = do
  i <- parseInt
  char '.'
  f <- many1 digit
  exp <- option "" parseExp -- get exponent (including sign) if it is there
  return (read (show i ++ "." ++ f ++ exp) :: Double)
  <?> "floating-point number"

-- Parse an exponent for a floating point number.
parseExp :: Parser [Char]
parseExp = do
  exp <- oneOf "eE"             -- get exponent
  sign <- option "" parseSign   -- get optional sign
  f <- many1 digit              -- must have at least one digit as exponent
  return ([exp] ++ sign ++ f)
  <?> "exponent"

-- Parse a sign (+ or -)
parseSign :: Parser [Char]
parseSign = do
  sign <- oneOf "-+"    -- get sign
  return [sign] -- return as a string
  <?> "sign"
          

parseId :: Parser String
parseId = do
  many1 (alphaNum <|> oneOf "_+-*/=?!") <?> "identifier"

-- Parse a string, which is a sequence of any characters surrounded by
-- double quotes.
parseString :: Parser String
parseString = do 
  char '\"'              -- start with a double quote
  x <- many (noneOf "\"")  -- get all characters between double quotes
  char '\"'         -- end with a double quote
  return x          -- return string in between double quotes
  <?> "string"

parseAtom :: Parser Atom
parseAtom =
  (parseBool >>= return . BoolA)
  <|> try (parseFloat >>= return . FloatA)
  <|> try (parseInt >>= return . IntA)
  <|> try (parseString >>= return . StringA) --look for string before identifier
  <|> (parseId >>= return . IdA)
  <?> "atom"

parseComment :: Parser ()
parseComment = do
  char ';'
  many (noneOf "\n")
  char '\n'
  return ()

parseWhitespace :: Parser ()
parseWhitespace = many1 space >> return ()

-- Parse a separator (whitespace or comment).
parseSep :: Parser ()
parseSep = 
  many1 (parseComment <|> parseWhitespace) >> return ()
  <?> "separator"

-- Parse a list of S-expressions, delimited by (), [], or {}
-- separated by whitespace/comments.

-- Parse a list of S-expressions, delimited by parentheses, square brackets, or
-- curly braces, and separated by whitespace/comments.
parseList :: Parser [Sexpr]
parseList = 
  parseDelimiters '(' ')'  -- call helper function with all possible delimiters
  <|> parseDelimiters '[' ']'  -- no need to "try", since we can already tell by 1st char
  <|> parseDelimiters '{' '}' -- by first char if '(', '[', or '{'.
  <?> "list of S-expressions"

-- Helper function to parse a list of Sexpr, seperated by comments or
-- whitespace, and surrounded by the character n on the left and the 
-- character m on the right.
parseDelimiters :: Char -> Char -> Parser [Sexpr]
parseDelimiters n m = do
  char n
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  char m
  return ss
  <?> "list of S-expressions"


-- Parse a quoted expression.
parseQuote :: Parser Sexpr
parseQuote = do
  char '\''    -- quote starts with a ' symbol
  x <- parseSexpr  -- get the quoted expression
  return (ListS [AtomS (IdA "quote"), x])  -- return as a list
  <?> "quoted S-expression"

-- Parse a single S-expressions.
parseSexpr :: Parser Sexpr
parseSexpr = 
  (parseAtom >>= return . AtomS)
  <|> (parseList >>= return . ListS)
  <|> parseQuote
  <?> "S-expression"

-- Parse a series of Sexprs from a string representing the entire contents of a
-- file.
parseSexprsFromFile :: Parser [Sexpr]
parseSexprsFromFile = do
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  eof
  return ss
  <?> "file of S-expressions"

----------------------------------------------------------------------
-- Pretty-printer.
----------------------------------------------------------------------

indent :: Int -> String
indent i = replicate i ' '

-- Pretty-print a Sexpr.
ppSexpr :: Int -> Sexpr -> String
ppSexpr i (AtomS a)  = indent i ++ show a
ppSexpr i (ListS ss) = 
  indent i
  ++ "ListS[\n" 
  ++ concatMap (\s -> ppSexpr (i + 2) s ++ "\n") ss
  ++ indent i ++ "]"

-- Parse all expressions in a file and run the pretty-printer on them.
runPpSexpr :: FilePath -> IO ()
runPpSexpr f = do
  p <- parseFromFile parseSexprsFromFile f
  case p of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right ss -> 
      mapM_ (\s -> do
        putStrLn (ppSexpr 0 s)
        putStrLn "") ss

----------------------------------------------------------------------
-- Tests.
----------------------------------------------------------------------

test :: IO ()
test = runPpSexpr "test.scm"

