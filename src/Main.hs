module Main where

import Tokenizer(tokenize)

main::IO()

main = do 
         str <- getContents
         print (tokenize str)
        
      
