import Control.Monad
import Data.List
import Data.Char
import Data.Maybe

type Board = [Int]

cand :: [Int] -> Int -> [Int]
cand b p = [1..9] \\ nub ([b!!(9*px+y)|y<-[0..8]]++[b!!(9*x+py)|x<-[0..8]]
                           ++[b!!(9*x+y)|x<-[i..(i+2)],y<-[j..(j+2)]])
       where
         [px,py] = [div p 9, mod p 9]
         [i,j]   = [(div px 3)*3, (div py 3)*3]

fill :: Int -> Board -> [Board]   
fill p b | b !! p /= 0 = [b] 
         | otherwise   = [ setCell p c b | c <- cand b p ]

setCell :: Int -> Int -> Board -> Board
setCell p c b = take p b ++ [c] ++ drop (p + 1) b

-- fill 0 >=> (fill 1 >=> ( ... >=> (fill 80 >=> pure)))
--   ==  b  →  fill 0  →  fill 1  →  ...  →  fill 80  →  pure
solver :: Board -> [Board]
solver = foldr (>=>) pure (map fill [0..80])

main = do
  line <- getLine
  print (listToMaybe (solver (map (\x -> ord x - ord '0') line)))


-- echo 530070000600195000098000060800060003400803001700020006060000280000419005000080079 | ./sudoku_monad
