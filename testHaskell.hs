module Main (main) where

import ParseSVM

main = do
  input <- "/* initialize a list of 10 elements set to -1 */\n\n#loop\nmov reg1 reg2\nadd reg1 10\nmov [reg1] -1\nadd reg2 1\nmov reg3 reg2\ncmp reg3 10\njeq end reg3\njmp loop\n#end" ::String
  let result = parse program "" input
  print(result)
