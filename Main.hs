module Main (main) where

import System.IO
import System.Environment
import System.Process

import Pond.Backend
import Pond.Frontend


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> print "Please pass the path to a source file\n"
        [file] -> do
            handle <- openFile (args !! 0) ReadMode
            file <- hGetContents handle

            let ast = parseSource file

            writeFile "Build/out.s" $ compile ast

            hClose handle
