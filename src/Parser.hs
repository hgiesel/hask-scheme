module Parser where

import Data.Function
import SchemeVal ( SchemeVal(..) )

import Control.Monad ( liftM )

import qualified Text.Parsec as Ps
import Text.Parsec.Char ( string
                        , anyChar
                        )

spaces :: Ps.Parsec String () ()
spaces = Ps.skipMany1 Ps.space

symbol :: Ps.Parsec String () Char
symbol = Ps.oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case Ps.parse parseExpr "Lisp" input of
  Left  err -> "No match: " ++ show err
  Right val -> "Found value: " ++ (show val)

parseExpr :: Ps.Parsec String () SchemeVal
parseExpr = parseSimpleExpression Ps.<|> parseParenList

----

parseSimpleExpression :: Ps.Parsec String () SchemeVal
parseSimpleExpression = parseString
                 Ps.<|> parseChar
                 Ps.<|> parseNumber
                 Ps.<|> parseAtom
                 Ps.<|> parseQuoted

parseParenList :: Ps.Parsec String () SchemeVal
parseParenList = do
  let dottedOrNotList = Ps.try parseList Ps.<|> parseDottedList
  x <- Ps.between (Ps.char '(') (Ps.char ')') dottedOrNotList
  return x

parseString :: Ps.Parsec String () SchemeVal
parseString = do
  let dQuote        = Ps.char '"'
  let stringContent = Ps.many $ (Ps.string "\\\"" >> return '"') 
                         Ps.<|> (Ps.string "\\n"  >> return '\n')
                         Ps.<|> (Ps.string "\\r"  >> return '\r')
                         Ps.<|> (Ps.string "\\t"  >> return '\t')
                         Ps.<|> (Ps.string "\\v"  >> return '\v')
                         Ps.<|> (Ps.string "\\a"  >> return '\a')
                         Ps.<|> (Ps.string "\\\\"  >> return '\\')
                         Ps.<|> (Ps.noneOf $ ['\"','\n','\r','\t','\v','\a','\\'])
  x <- Ps.between dQuote dQuote stringContent
  return . SString $ x

parseAtom :: Ps.Parsec String () SchemeVal
parseAtom = do
  first <- Ps.letter Ps.<|> symbol
  rest  <- Ps.many (Ps.letter Ps.<|> Ps.digit Ps.<|> symbol)

  let atom = first:rest
  return $  case atom of
    "#t" -> SBool True
    "#f" -> SBool False
    _    -> SAtom atom

parseChar :: Ps.Parsec String () SchemeVal
parseChar = do
  Ps.char '#'
  Ps.char '\\'
  x <- (Ps.string "space" >> return ' ') Ps.<|> (Ps.string "newline" >> return '\n') Ps.<|> Ps.anyChar
  return . SChar $ x

parseNumber :: Ps.Parsec String () SchemeVal
parseNumber = liftM (SNumber . read) $ Ps.many1 Ps.digit

parseNumber' :: Ps.Parsec String () SchemeVal
parseNumber' = do
  x <- Ps.many1 Ps.digit
  return $ SNumber . read $ x

parseList :: Ps.Parsec String () SchemeVal
parseList = do
  l <- Ps.sepBy parseExpr Ps.spaces
  return . SList $ l

parseDottedList :: Ps.Parsec String () SchemeVal
parseDottedList = do
  init <- Ps.endBy parseExpr Ps.spaces
  Ps.char '.' >> Ps.spaces

  last <- parseExpr
  return $ SDottedList init last

parseQuoted :: Ps.Parsec String () SchemeVal
parseQuoted = do
  Ps.char '\''
  x <- parseExpr
  return $ SList [SAtom "quote", x]
