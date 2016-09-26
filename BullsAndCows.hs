
-- A simple game to guess the letters in a word by tiral and error.
-- Start a game with `bullsAndCows "yourwordhere"` then let an opponent try to guess

-- A Bull is when the guess string contains a letter at the same position as the secret word
-- A Cow is when the guess string contains a letter in the secret word, but in the wrong position

checkCow :: String -> String -> Int
-- a cow is when a letter from the guess is in the word
checkCow guess word
  = let inter = map (\x -> elem x word) guess
    in foldr (+) 0 (map (\x -> if x == True then 1 else 0) inter)


checkBull :: String -> String -> Int
-- a bull is when a letter from the guess is in the same position in the word
checkBull guess word
  = foldr (+) 0 [ (if x == y then 1 else 0) | (x,y) <- zip guess word ]


check :: String -> String -> (Int, Int)
-- given a guess and a string, produce the number of bulls and cows in the guess
check guess word
  = let cows = checkCow guess word
        bulls = checkBull guess word
    in (bulls, cows - bulls)


mkguess :: String -> (Int, Int) -> IO ()
mkguess word (bulls, cows) =
  do putStrLn (show bulls ++ " bulls, and " ++ show cows ++ " cows")
     putStr " Enter your guess: "
     q <- getLine
     turn word (check q word)


turn :: String -> (Int, Int) -> IO ()       
turn word (bulls, cows) =
  do if bulls == length word
       then putStrLn "You Win!"
       else mkguess word (bulls, cows)

bullsAndCows :: String -> IO ()
bullsAndCows word
  = turn word (0, 0)
       
