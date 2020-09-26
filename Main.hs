module Main (main) where

import System.IO
import System.Environment
import System.Process

import Pond.Frontend
import Pond.Backend

getTree :: IO ()
getTree = do
            handle <- openFile "Examples/test.pnd" ReadMode
            file <- hGetContents handle
            let ast = parseSource file
            print ast


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> print "Please pass the path to a source file\n"
        [path] -> do
            handle <- openFile path ReadMode
            file <- hGetContents handle

            let ast = parseSource file

            writeFile "Build/out.s" $ compile ast

            hClose handle
