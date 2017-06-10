module Main (main) where

import ParseSVM

-- parses SVM file from standard input port and displays result
main = do
  input <- getContents -- reads from stdin
  print(input)
  let result = parse program "(standard input)" input
  print result
