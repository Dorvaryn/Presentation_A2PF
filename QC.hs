module Main where

import System.Environment
import System.Cmd
import Data.List

main :: IO ()
main = do { as <- getArgs
          ; mapM_ process as }

process :: String -> IO ()
process file = do { cts <- readFile file
                  ; let tests = getTests cts
                  ; if null tests then
                        putStrLn (file ++ ": no properties to check")
                    else do

                    { writeFile "script" $
                        unlines ([":l " ++ file] ++ [":m +Test.QuickCheck"]++ concatMap makeTest tests)
                    ; system ("ghci -v0 < script")
                    ; system ("rm script")
                    ; return () }}

getTests :: String -> [String]
getTests cts = nub $ filter ("prop_" `isPrefixOf`) $
               map (fst . head . lex) $ lines cts

makeTest :: String -> [String]
makeTest test = ["putStr \"" ++ test ++ ": \"", "quickCheck " ++ test]
