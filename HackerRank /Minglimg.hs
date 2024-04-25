mingling :: String -> String -> String
mingling [] [] = []
mingling (x:xs) (y:ys) = x:y:mingling xs ys

main :: IO ()
main = do
    pawel <- getLine
    shaka <- getLine
    let mingled = mingling pawel shaka
    putStrLn mingled