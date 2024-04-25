module Main where

import Scanner(scanner)
import Parser (pSlides)
import UU.Parsing
import ToHtml
main :: IO ()
main = do input <- readFile "slide.p5"
          let token = scanner input
          putStrLn(show token) 
          tree <- parseIO pSlides token
          putStrLn(show tree)
          putStrLn (slidesToHtml tree)
          writeFile "index.html" (slidesToHtml tree)