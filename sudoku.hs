import Control.Monad
import Data.List
import Data.Char

-- returns list of candidate for the position: p
cand :: [Int] -> Int -> [Int]
cand b p = [0..9] \\ nub ([b!!(9*px+y)|y<-[0..8]]++[b!!(9*x+py)|x<-[0..8]]
                           ++[b!!(9*x+y)|x<-[i..(i+2)],y<-[j..(j+2)]])
       where
         [px,py] = [div p 9, mod p 9]
         [i,j]   = [(div px 3)*3, (div py 3)*3]

head' x | x == [] = []
        | otherwise = head x

-- b: board, p: position
solver :: [Int] -> Int -> [Int]
solver b p | p > 80      = b
           | b!!p == 0   = head' $ filter (/=[]) [solver ((take p b)++[c]++(drop (p+1) b)) (p+1)| c<-(cand b p)]
           | otherwise   = solver b (p+1)

main = do 
	forever $ do
		line <- getLine
		print $ solver (map (\x -> ((ord x)-(ord '0'))) line) 0
	
