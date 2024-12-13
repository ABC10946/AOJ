module Main where

import Control.Applicative

gcd' :: (Integral a) => a -> a -> a
gcd' a b 
    | a `mod` b == 0 = b
    | otherwise      = gcd' b (a `mod` b)


main :: IO ()
main = do
    [a,b] <- map read . words <$> getLine :: IO [Int]
    putStrLn $ show (gcd' a b)
