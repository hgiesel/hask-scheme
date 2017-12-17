module Eval where

import SchemeVal ( SchemeVal(..)
                 , schemeVal
                 )
import Data.Map ( Map(..)
                , fromList
                , (!) )
import Data.Bool ( bool )


eval :: SchemeVal -> SchemeVal
eval val@(SString _) = val
eval val@(SChar _)   = val
eval val@(SNumber _) = val
eval val@(SBool _)   = val
eval (SList [SAtom "quote", val]) = val
eval (SList (SAtom func:args))    = apply func . fmap eval $ args
eval _                            = SBool False

type SchemeFunc = [SchemeVal] -> SchemeVal

apply :: String -> [SchemeVal] -> SchemeVal
apply name = primitives ! name

primitives :: Map String SchemeFunc
primitives = fromList [ ("+"        , numericBinop (+))
                      , ("-"        , numericBinop (-))
                      , ("*"        , numericBinop (*))
                      , ("/"        , numericBinop div)
                      , ("mod"      , numericBinop mod)
                      , ("quotient" , numericBinop quot)
                      , ("remainder", numericBinop rem)
                      , ("and"      , logicBinop (&&))
                      , ("or"       , logicBinop (||))
                      , ("symbol?"  , SBool . isSymbol . head)
                      , ("string?"  , SBool . isString . head)
                      , ("number?"  , SBool . isNumber . head)
                      , ("char?"    , SBool . isChar . head)
                      ]

numericBinop :: (Integer -> Integer -> Integer) -> SchemeFunc
numericBinop op = SNumber . foldr1 op . fmap unpackNum

logicBinop :: (Bool -> Bool -> Bool) -> SchemeFunc
logicBinop op = SBool . foldr1 op . fmap unpackBool

isSymbol :: SchemeVal -> Bool
isSymbol (SAtom _) = True
isSymbol _         = False

isString :: SchemeVal -> Bool
isString (SString _) = True
isString _           = False

isNumber :: SchemeVal -> Bool
isNumber (SNumber _) = True
isNumber _           = False

isChar   :: SchemeVal -> Bool
isChar (SChar _) = True
isChar _         = False

isBool :: SchemeVal -> Bool
isBool (SBool _) = True
isBool _         = False

unpackNum :: SchemeVal -> Integer
unpackNum = schemeVal zero
                      (\x -> let parsed = reads x in
                        bool (fst $ head parsed) 0 $ null parsed)
                      zero
                      id
                      zero
                      (\y -> bool 0 (head y) $ length y == 1)
                      zero'
                        where zero  = const 0
                              zero' = const . const $ 0

unpackBool :: SchemeVal -> Bool
unpackBool = schemeVal false
                       false
                       false
                       (toEnum . fromInteger)
                       id
                       false
                       false'
                         where false  = const False
                               false' = const . const $ False
