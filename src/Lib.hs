module Lib where


import qualified Eval      as Ev
import qualified Parser    as Pr
import qualified SchemeVal as Sv

import System.Environment ( getArgs )
import Control.Monad ( forever )
import System.IO ( putStr
                 , getLine
                 , hFlush
                 , stdout
                 )

import System.Exit ( exitSuccess
                   )

import Data.Bool ( bool
                 )

colorMagenta :: String -> String
colorMagenta = Sv.surroundWith "\x1b[1;35m" "\x1b[0m"

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

parseCommands :: IO ()
parseCommands = forever $ do
  input <- readPrompt . colorMagenta $ "repl>> "
  bool (putStrLn . show . Ev.eval . Pr.readExpr $ input) exitSuccess $ input == ":q"
