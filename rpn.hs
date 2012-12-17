import System.Environment

main = do
    expression <- getArgs
    putStrLn $ show $ rpn expression

--rpn :: [String] -> Int
--rpn expression = rpn' expression [] where
--    rpn' []         stack = head stack
--    rpn' ("*":expr) (x:y:stack) = rpn' expr (x*y : stack)
--    rpn' ("/":expr) (x:y:stack) = rpn' expr (x/y : stack)
--    rpn' ("+":expr) (x:y:stack) = rpn' expr (x+y : stack)
--    rpn' ("-":expr) (x:y:stack) = rpn' expr (x-y : stack)
--    rpn' (x:expr  ) stack       = rpn' $ (read x) : stack


rpn :: [String] -> Double
rpn = head . foldl calculate []
    where calculate (x:y:stack) "*" = (y * x):stack
          calculate (x:y:stack) "/" = (y / x):stack
          calculate (x:y:stack) "+" = (y + x):stack
          calculate (x:y:stack) "-" = (y - x):stack
          calculate stack       num = read num : stack

rpnW :: String -> Double
rpnW = rpn . words
