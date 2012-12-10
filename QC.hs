module Main where

import System.Environment
import System.Cmd
import Data.List

main :: IO ()
-- Main just process every arguments
main = do { as <- getArgs
          ; mapM_ process as }

process :: String -> IO ()
-- Where things are done,
-- write a script file executing quickCheck for every properties in the given file
process file = do { cts <- readFile file
                  ; let tests = getTests cts
                  ; if null tests then
                        putStrLn (file ++ ": no properties to check")
                    else do

                    { writeFile "script" $
                        unlines ([":l " ++ file] ++ [":m +Test.QuickCheck"] ++ ["putStr \"Checking : " ++ file ++ "\\n\""] ++ concatMap makeTest tests ++ ["putStr \"\\n\""])
                    ; system ("ghci -v0 < script")
                    ; system ("rm script")
                    ; return () }}

getTests :: String -> [String]
-- Extracts properties names
getTests cts = nub $ filter ("prop_" `isPrefixOf`) $
               map (fst . head . lex) $ lines cts

makeTest :: String -> [String]
-- Write the command to execute quickCheck for the given property
makeTest test = ["putStr \"" ++ test ++ ": \"", "quickCheck " ++ test]
