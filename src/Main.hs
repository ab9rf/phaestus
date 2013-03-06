module Main where

import PHPLex (parse)
import PHPParse (phpParse)

main::IO()

main =  do
        str <- getContents
        print (parse phpParse str)
        
      
