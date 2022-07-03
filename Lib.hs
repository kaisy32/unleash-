module Lib
    (parseNums,
    parseOps,
    eval,
  
    ) where
import Control.Monad
import Control.Lens
import Data.Maybe (fromJust)
import Data.List (unfoldr,splitAt)
import Data.Char (isNumber)
import Data.Foldable (traverse_)

isOp::Char -> Bool 
isOp x = x `elem` "+-*/"


parseOps :: [Char] -> Maybe ([Int], [Char])
parseOps "" = Nothing
parseOps str = Just ( filter(>=0) $ (map (\z -> if isOp (str!!z) then z else -1) $ [0..(length str)-1] ),
                      filter(\x -> x/='k')  $ map (\z -> if isOp (str!!z) then str!!z else 'k') $ [0..(length str)-1] )


--splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
--splitBy delimiter = foldr f [[]] 
--            where f c l@(x:xs) | c == delimiter = []:l
--                             | otherwise = (c:x):xs
                             
parseNums :: String -> Maybe [String]
parseNums "" = Nothing
parseNums str =
  case span f str of
    ("", (_:ss)) -> parseNums ss
    (num, rest) ->
      Just $ num : case parseNums rest of
        Nothing -> []
        Just smth -> smth
  where 
    f c = isNumber c && not (c `elem` operators)
    operators = ['+', '-', '*', '/']

text = "5+2+2030*2"
a=parseOps text 
b=parseNums text

eval :: [Char] ->  [String] -> Int -> Int  -> Int -> Maybe Int
eval s1 s2 pos1 pos2 ev = do
      if(pos1 <= (length s1)-1 && pos2<=(length s2)-1 ) then
        case (s1!!pos1) of 
          '+' -> (eval s1 s2 (pos1+1) (pos2+1 ) (ev + ( (read (s2!!pos2) :: Int))))
          '-' -> (eval s1 s2 (pos1+1) (pos2+1 ) (-ev - ( (read (s2!!pos2) :: Int))))
          '*' -> (eval s1 s2 (pos1+1) (pos2+1 ) (ev + ( (read (s2!!pos2) :: Int))))
          _   -> Nothing
      else
        case (s1!!((length s1)-1)) of 
            '+' -> Just $ (ev +  ( (read (s2!!pos2) :: Int)))
            '-' -> Just $ (-ev - (( (read (s2!!pos2) :: Int))))
            '*' -> Just $ (ev *  ( (read (s2!!pos2) :: Int)))
            _   -> Nothing