module HelloWorldNTimes where

helloWorldNTimes :: Int -> IO ()
helloWorldNTimes n
    | n <= 0 = return ()
    | otherwise = do
            putStrLn "Hello World"
            helloWorldNTimes (n - 1)

main :: IO ()
main = do
    n <- readLn :: IO Int
    helloWorldNTimes n
