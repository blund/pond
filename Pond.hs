{-# LANGUAGE BangPatterns #-}

-- https://wiki.haskell.org/Handling_errors_in_Haskell

module Main (main) where

import System.IO
import System.Environment
import System.Process

import Backend
import Frontend
import Parser


main :: IO ()
main = do
    mode <- getArgs

    handle <- openFile "test.c" ReadMode
    file <- hGetContents handle

    let ast = extract $ parse program file

    case mode of
        ["compile"] -> do
            writeFile "test.s" $ compile ast
        otherwise -> do
            print ast
            -- print $ execute ast

    hClose handle
