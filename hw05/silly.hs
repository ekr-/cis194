
sillyExchange :: IO ()
sillyExchange = do
  putStrLn "Hello, user!"
  putStrLn "What's your name?"
  name <- getLine
  putStrLn $ "Pleased to meet you, " ++ name ++ "!"

main = sillyExchange
