import System.IO
import System.Environment (getArgs)

swapOnce :: Char -> Char -> [Char] -> [Char]
swapOnce _ _ [] = []
swapOnce x y (z:zs)
    | x == z = y : zs
    | otherwise = z : swapOnce x y zs

larger :: [Char] -> [Char]
larger [] = []
larger [x] = [x]
larger (x:xs)
    | x >= customMax = x : larger xs
    | otherwise = customMax : (swapOnce customMax x xs)
    where customMax = maximum xs

smaller :: Bool -> [Char] -> [Char]
smaller y [] = []
smaller y [x] = [x]
smaller y (x:xs)
    | [x | x <- xs , x > '0'] == [] = x : xs
    | x <= myMin = x : smaller False xs
    | otherwise = myMin : (reverse (swapOnce myMin x (reverse xs)))
    where myMin  = customMin y xs

customMin :: Bool -> [Char] -> Char
customMin x y
    | x == True = minimum [x | x <- y, x > '0']
    | x == False = minimum y

main = do
    [inpFile] <- getArgs
    inh <- openFile inpFile ReadMode
    mainloop inh
    hClose inh

mainloop :: Handle -> IO ()
mainloop inh = 
    do ineof <- hIsEOF inh
       if ineof
           then return ()
           else do inpStr <- hGetLine inh
                   readloop [1..(read inpStr :: Integer)] inh
                   mainloop inh

readloop :: [Integer] -> Handle -> IO ()
readloop [] inh = return ()
readloop (x:xs) inh = do inpStr <- hGetLine inh
                         let statement = "Case #" ++ (show x) ++ ": " ++ (smaller True inpStr ) ++ " " ++ larger inpStr
                         putStrLn statement 
                         readloop xs inh

-- Case #1: 13524 51324
-- Case #2: 798 987
-- Case #3: 123 321
-- Case #4: 10 10
-- Case #5: 5 5