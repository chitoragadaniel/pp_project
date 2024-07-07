module Main where
import CodeGen
import Sprockell (run)
import Elaborator (optimizeProgram, checkProgram)
import Parser (runParseProgram)
import Data.List.Split

main :: IO ()
main = do
  text <- readFile "./test/testCode"
  runFile "./test/demos/p2"