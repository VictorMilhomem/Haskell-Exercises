module Main where


lastOne :: [a] -> a 
lastOne xs = case xs of
  [] -> error "Empty List"
  [x] -> x 
  (_:xs) -> lastOne xs


lastTwo :: [a] -> [a]
lastTwo xs = case xs of
  [] -> error "Empty List"
  [x] -> [x]
  [x,y] -> [x,y]
  (_:xs) -> lastTwo xs


nth :: Int -> [a] -> a
nth n xs
  | n < 0 = error "Index out of bounds"
  | otherwise = case xs of
    [] -> error "Empty List"
    (y:ys) -> if n == 0 then y else nth (n-1) ys


len :: [a] -> Int
len xs = case xs of 
  [] -> 0 
  (_:xs) -> 1 + len xs 


rev :: [a] -> [a]
rev xs = case xs of
  [] -> error "Empty List"
  [x] -> [x]
  (x:xs) -> rev xs ++ [x]


palindrome :: Eq a => [a] -> Bool
palindrome xs 
  | xs == rev xs = True
  | otherwise = False


compress :: Eq a => [a] -> [a]
compress xs = case xs of 
  [] -> error "Empty List"
  [x] -> [x]
  (x:y:xs) -> if x == y then compress (y:xs) else x : compress (y:xs)


pack :: Eq a => [a] -> [[a]]
pack xs = case xs of 
  [] -> error "Empty List"
  [x] -> [[x]]
  (x:xs) -> pack' xs [x]

pack' :: Eq a => [a] -> [a] -> [[a]]
pack' xs acc = case xs of
  [] -> [acc]
  (x:xs)-> if x == head acc then pack' xs (x:acc) else acc : pack' xs [x]


main :: IO ()
main = do
  print $ palindrome ['x', 'a', 'm', 'a', 'x'] 
  putStrLn "Hello World"
