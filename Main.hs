module Main (main) where

import System.IO
import System.Environment
import System.Process

import Pond.Backend
import Pond.Frontend


runCompile = do

    handle <- openFile "Examples/test.c" ReadMode
    file <- hGetContents handle

    let ast = parseSource file

    print ast

    writeFile "out.s" $ compile ast

    hClose handle


main :: IO ()
main = do
    args <- getArgs

    handle <- openFile (args !! 0) ReadMode
    file <- hGetContents handle

    let ast = parseSource file

    writeFile "Build/out.s" $ compile ast

    hClose handle
