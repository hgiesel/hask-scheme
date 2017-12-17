{-# LANGUAGE GADTs, InstanceSigs #-}

module SchemeVal where

import qualified Text.Parsec as Ps

import Data.Semigroup ( (<>)
                      , Semigroup
                      )

import Data.Bool ( bool
                 )

data SchemeVal where
  SAtom       :: String                   -> SchemeVal
  SString     :: String                   -> SchemeVal
  SChar       :: Char                     -> SchemeVal
  SNumber     :: Integer                  -> SchemeVal
  SBool       :: Bool                     -> SchemeVal
  SList       :: [SchemeVal]              -> SchemeVal
  SDottedList :: [SchemeVal] -> SchemeVal -> SchemeVal

schemeVal :: (String   -> a) ->
             (String   -> a) ->
             (Char     -> a) ->
             (Integer  -> a) ->
             (Bool     -> a) ->
             ([a]      -> a) ->
             ([a] -> a -> a) ->
             SchemeVal -> a
schemeVal f _ _ _ _ _ _ (SAtom   v)       = f v
schemeVal _ g _ _ _ _ _ (SString v)       = g v
schemeVal _ _ l _ _ _ _ (SChar   v)       = l v
schemeVal _ _ _ h _ _ _ (SNumber v)       = h v
schemeVal _ _ _ _ i _ _ (SBool   v)       = i v
schemeVal f g l h i j k (SList   v)       = j ((fmap $ schemeVal f g l h i j k) v)
schemeVal f g l h i j k (SDottedList v w) = k ((fmap $ schemeVal f g l h i j k) v) (schemeVal f g l h i j k w)

instance Show SchemeVal where
  show = showSchemeVal

showSchemeVal :: SchemeVal -> String
showSchemeVal = schemeVal id
                          (surroundWith "\"" "\"")
                          show
                          show
                          (bool "#f" "#t")
                          ((surroundWith "(" ")") . unwords)
                          (\x y -> "(" <> (unwords x) <> " . " <> y <> ")")

surroundWith :: Semigroup a => a -> a -> a -> a
surroundWith l r = (l <>) . (<> r)
