import Text.Printf
import Rose.Rose
import Data.List (sort)


data Small = Small Int Int Int Int Int Int
data Turn =  Player | Computer deriving (Show, Eq)
data Mancala = Mancala {turn :: Turn, pBig :: Int, cBig :: Int, pSmall :: Small, cSmall :: Small}

instance Show Small where
    show (Small a b c d e f) = printf "   %2d %2d %2d %2d %2d %2d\n" a b c d e f

adjust :: Int -> String
adjust a
       | a>=10 = show a
       | otherwise = " " ++ show a 

instance Show Mancala where
    show (Mancala turn pBig cBig pSmall cSmall) = printf $ "\n" ++ show cSmall ++ adjust cBig ++ "                   " ++ show pBig ++ "\n"
                                                         ++ show pSmall

valid :: Mancala -> [Int]
valid (Mancala turn pBig cBig pSmall cSmall)
        | turn == Player = [i | i <- [1..6], let (Small a b c d e f) = pSmall, let x = case i of
                                                                                      1 -> a
                                                                                      2 -> b
                                                                                      3 -> c
                                                                                      4 -> d
                                                                                      5 -> e
                                                                                      6 -> f, x > 0]
        | turn == Computer = [i | i <- [12,11..7], let (Small a b c d e f) = cSmall, let x = case i of
                                                                                              12 -> a
                                                                                              11 -> b
                                                                                              10 -> c
                                                                                              9 -> d
                                                                                              8 -> e
                                                                                              7 -> f, x > 0]

shift :: Int -> [a] -> [a]
shift _ [] = []
shift n xs = drop (n `mod` length xs) xs ++ take (n `mod` length xs) xs

doMove :: Mancala -> Int -> Mancala
doMove (Mancala turn pBig cBig pSmall cSmall) n = if ((not $ isGameOver (Mancala turn pBig cBig pSmall cSmall))  -- da li mora da se proverava game state jer ako je kraj igre onda svakako nece biti mogucih poteza
                                                        && n `elem` (valid (Mancala turn pBig cBig pSmall cSmall)))
                                                  then handle (Mancala turn pBig cBig pSmall cSmall) n else error "Los input"
        where
            fromSmall (Small a b c d e f) = [a,b,c,d,e,f]
            toSmall [a,b,c,d,e,f] = (Small a b c d e f)
            pebbles xs val = map (+1) (take val xs) ++ drop val xs
            steal idx xs = if xs !! idx == 1 && idx < 6 && xs !! (12-idx) /=0 
                           then take idx xs ++ [0] ++ (take (5-idx) $ drop (idx+1) xs) ++ 
                                [xs !! 6 + xs !! (12-idx) + xs !! idx] ++ 
                                (take (5-idx) $ drop 7 xs) ++ [0] ++ (drop (13-idx) xs)
                           else xs
            clean (Mancala turn pBig cBig (Small 0 0 0 0 0 0) cSmall) = (Mancala turn pBig ((+) cBig $ sum . fromSmall $ cSmall) (Small 0 0 0 0 0 0) (Small 0 0 0 0 0 0))
            clean (Mancala turn pBig cBig pSmall (Small 0 0 0 0 0 0)) = (Mancala turn ((+) pBig $ sum . fromSmall $ pSmall) cBig  (Small 0 0 0 0 0 0) (Small 0 0 0 0 0 0))
            clean (Mancala turn pBig cBig pSmall cSmall) = (Mancala turn pBig cBig pSmall cSmall)
            flip Player = Computer
            flip Computer = Player
            ps = fromSmall pSmall
            cs = reverse . fromSmall $ cSmall
            big = if turn == Player then pBig else cBig        
            handle (Mancala turn pBig cBig pSmall cSmall) n   = do let xs = if turn == Player then ps else cs
                                                                   let idx = if turn == Player then n else n-6
                                                                   let val = xs !! (idx-1) `mod` 13
                                                                   let val' = xs !! (idx-1) `div` 13
                                                                   let whole = (take (idx-1) xs) ++ [0] ++ (drop idx xs) ++ [big] ++ (if turn == Player then cs else ps)
                                                                   let newState = steal ((idx + val - 1) `mod` 13) $ map (+val') $ shift (13-idx) $ pebbles (shift idx whole) val
                                                                   let newTurn = if val + idx == 7 then turn else flip turn
                                                                 
                                                                   if turn == Player 
                                                                    then clean (Mancala newTurn (head . drop 6 $ newState) cBig
                                                                    (toSmall $ take 6 newState) (toSmall $ take 6 (reverse newState)))

                                                                    else clean (Mancala newTurn pBig  (head . drop 6 $ newState)
                                                                      (toSmall . reverse . take 6 $ reverse newState) (toSmall . reverse . take 6 $ newState))                                                                

isGameOver :: Mancala -> Bool
isGameOver (Mancala turn pBig cBig pSmall cSmall) = if (sum ((fromSmall pSmall) ++ (fromSmall cSmall))) == 0
                                                    then True else False
                                                    where fromSmall (Small a b c d e f) = [a,b,c,d,e,f]
 
getWinner :: Mancala -> Int
getWinner (Mancala turn pBig cBig pSmall cSmall) = if pBig>cBig
                                                    then 1 else if pBig == cBig then 0 else -1
                                               

genMoves :: Rose (Mancala,Int) -> Int -> Rose (Mancala,Int)
genMoves (Node ((Mancala turn pBig cBig pSmall cSmall),move) xs) n
        | isGameOver (Mancala turn pBig cBig pSmall cSmall) = (Node ((Mancala turn pBig cBig pSmall cSmall), move) [])
        | n == 0 = (Node ((Mancala turn pBig cBig pSmall cSmall), move) xs)
        | otherwise = (Node ((Mancala turn pBig cBig pSmall cSmall), move) [genMoves (Node (doMove (Mancala turn pBig cBig pSmall cSmall) i, i) []) (n - 1) | i <- valid (Mancala turn pBig cBig pSmall cSmall)])

main = do
        let mancala = Mancala Computer 0 0 (Small 4 4 4 4 4 4) (Small 4 4 4 4 4 4)
        --let mancala = Mancala Computer 0 0 (Small 2 2 2 2 2 2) (Small 0 0 0 4 1 0)
        --putStrLn $ show $ elemsOnDepth (genMoves (Node (mancala,0) []) 3) 3
        --let niz = elemsOnDepth (genMoves (Node (mancala,0) []) 3) 3
        --putStrLn $ show $ foldl (\acc (tabla,move) -> if (cBig tabla > 0) then tabla:acc else acc) [] niz
        --putStrLn $ show $ length (foldl (\acc (tabla,move) -> if (cBig tabla > 0) then tabla:acc else acc) [] niz)
        --putStrLn $ show $ length (elemsOnDepth (genMoves (Node (mancala,0) []) 3) 3)
        --putStrLn $ show $ valid mancala
        --putStrLn $ show $ doMove mancala 2
        let rose = fmap (\(tabla,move) -> minmax tabla 4) $ elemsOnDepth (genMoves (Node (mancala,0) []) 1) 1
        --putStrLn $ show $ doMove mancala (snd rose)
        putStrLn $ show $ rose 

alterTable :: (Mancala,Int) -> (Int,Int, Turn)
alterTable ((Mancala turn pBig cBig pSmall cSmall),move) = (pBig-cBig,move,turn)

findMove :: Rose (Int,Int, Turn) -> (Int,Int)
findMove (Node (val,move,turn) []) = (val,move)
findMove (Node (val,move,turn) xs) = if move == 0 then if turn == Player then maximum (fmap findMove xs)
                                                                         else minimum (fmap findMove xs)
                                                  else (fst $ if turn == Player then maximum (fmap findMove xs)
                                                                                else minimum (fmap findMove xs), move)
                                      

minmax :: Mancala -> Int -> (Int,Int)
minmax (Mancala turn pBig cBig pSmall cSmall) depth = findMove $ fmap alterTable $ genMoves (Node ((Mancala turn pBig cBig pSmall cSmall),0) []) depth

hlpr rez
    | rez == 1 = "Pobedio si"
    | rez == -1 = "Izgubio si"
    | otherwise = "Izjednaceno je"
playBot = playBotH (Mancala Computer 0 0 (Small 4 4 4 4 4 4) (Small 4 4 4 4 4 4))
playBotH tabla
    | isGameOver tabla = do
            putStr "Igra je gotova. "
            putStrLn $ hlpr $ getWinner tabla
    | (turn tabla) == Player = do
            putStrLn "\nStanje table je: "
            putStrLn $ show $ tabla
            putStr "Unesite potez: "
            putStrLn $ show $ valid tabla
            potez <- getLine
            let newTabla = doMove tabla (read potez)
            playBotH newTabla
    | otherwise = do
            --putStr "Moguci botovi potezi: "  
            --putStrLn $ show $ valid tabla
            let botMove =  snd $ minmax tabla 5
            putStrLn $ show botMove
            let newTabla =  doMove tabla botMove
            playBotH newTabla


newtype GameStateOp a = GameStateOp { runGameStateOp :: Mancala -> (a, Mancala) }

instance Functor GameStateOp where
    fmap f (GameStateOp f') = GameStateOp (\tabla -> let (a, tabla') = f' tabla in (f a, tabla'))

-- f (a->b) -> f a -> f b // f ((mancala->(a,mancala)->(mancala->(b,mancala)) -> f (mancala->(a,mancala) -> f (mancala->(b,mancala))
instance Applicative GameStateOp where
    pure x = (GameStateOp (\tabla -> (x,tabla)))
    (GameStateOp f) <*> (GameStateOp g) = (GameStateOp (\tabla -> let (f', tabla') = f tabla;
                                                                                     (a, tabla'') = g tabla'
                                                                                  in (f' a, tabla'')))

instance Monad GameStateOp where
    --(>>=) :: m a -> (a -> m b) -> m b
    (GameStateOp f) >>= g = (GameStateOp (\tabla -> let (a, tabla') = f tabla;
                                                                       GameStateOp h = g a
                                                                    in (h tabla')))

newtype GameStateOpHistory a = GameStateOpHistory { runGameStateOpHistory :: Mancala -> (a, [Mancala]) }

instance Functor GameStateOpHistory where
    fmap f (GameStateOpHistory f') = GameStateOpHistory (\tabla -> let (a, tabla') = f' tabla in (f a, tabla'))

instance Applicative GameStateOpHistory where
    pure x = (GameStateOpHistory (\tabla -> (x,[tabla])))
    (GameStateOpHistory f) <*> (GameStateOpHistory g) = (GameStateOpHistory (\tabla -> let (f', tabla') = f tabla;
                                                                                                                 (a, tabla'') = g (head tabla')
                                                                                                              in (f' a, tabla' ++ tabla'')))

instance Monad GameStateOpHistory where
    (GameStateOpHistory f) >>= g = (GameStateOpHistory (\tabla -> let (a, tabla') = f tabla;
                                                                                            GameStateOpHistory h = g a;
                                                                                            (b,tabla'') = h (head tabla')
                                                                                         in (b, tabla''++tabla')))

runGameState :: (Bool,Mancala)
runGameState = runGameStateOp applyMoves mancalaInitialState
                                              where 
                                                mancalaInitialState = Mancala Player 0 0 (Small 4 4 4 4 4 4) (Small 4 4 4 4 4 4)
                                                applyMoves = do
                                                                applyMove 3
                                                                applyMove 6
                                                                applyMove 8
                                                                applyMove 11
                                                                applyMove 2
                                                                applyMove 6
                                                                applyMove 4
                                                                applyMove 7
                                                                applyMove 9
                                                                applyMove 1
                                                                applyMove 3
                                                                applyMove 4
                                                                applyMove 6
                                                                applyMove 11
                                                                applyMove 12
                                                                applyMove 6
                                                                applyMove 1
                                                                applyMove 9
                                                                applyMove 2
                                                                applyMove 8
                                                                applyMove 4
                                                                applyMove 12
                                                                applyMove 9
                                                                applyMove 6
                                                                applyMove 3
                                                                applyMove 10
                                                                applyMove 6
                                                                applyMove 4
                                                                applyMove 11
                                                                applyMove 12

applyMove :: Int -> GameStateOp Bool
applyMove move = GameStateOp (\tabla -> let tabla' = doMove tabla move
                                                        in (isGameOver tabla', tabla'))


runGameStateH :: (Bool,[Mancala])
runGameStateH = runGameStateOpHistory applyMovesH mancalaInitialStateH
                                              where 
                                                mancalaInitialStateH = Mancala Player 0 0 (Small 4 4 4 4 4 4) (Small 4 4 4 4 4 4)
                                                applyMovesH = do
                                                                initialize
                                                                applyMoveH 3
                                                                applyMoveH 6
                                                                applyMoveH 8
                                                                applyMoveH 11
                                                                applyMoveH 2
                                                                applyMoveH 6
                                                                applyMoveH 4
                                                                applyMoveH 7
                                                                applyMoveH 9
                                                                applyMoveH 1
                                                                applyMoveH 3
                                                                applyMoveH 4
                                                                applyMoveH 6
                                                                applyMoveH 11
                                                                applyMoveH 12
                                                                applyMoveH 6
                                                                applyMoveH 1
                                                                applyMoveH 9
                                                                applyMoveH 2
                                                                applyMoveH 8
                                                                applyMoveH 4
                                                                applyMoveH 12
                                                                applyMoveH 9
                                                                applyMoveH 6
                                                                applyMoveH 3
                                                                applyMoveH 10
                                                                applyMoveH 6
                                                                applyMoveH 4
                                                                applyMoveH 11
                                                                applyMoveH 12
                                                                


initialize :: GameStateOpHistory Bool
initialize = GameStateOpHistory $ \s -> (False,[s])

applyMoveH :: Int -> GameStateOpHistory Bool
applyMoveH move = GameStateOpHistory (\tabla -> let tabla' = doMove tabla move
                                                            in (isGameOver tabla', [tabla']))

helper :: Mancala -> Int
helper (Mancala turn pBig cBig (Small q w e r t y) (Small a s d f g h)) = pBig + cBig + q + w + e +r +t+y+a+s+d+f+g+h

main2 = do
        let (res,xs) = runGameStateH 
        putStrLn $ show $  xs
        --putStrLn $ show $ map helper xs
        putStrLn $ show $ res
        putStrLn $ show $ length xs
        putStrLn $ show $ valid $ head xs