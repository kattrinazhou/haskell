
import Text.Parser
myParser = do 
  x <- char '1' <|> char '2' <|> char 'x'
  y <- char 'a' <|> char 'b'
   return [x,y]