import Control.Monad(mapM)

monadTest :: (Num a) => a -> [a]
{- Following two signatures are equivalent -}
-- monadTest val = return val
monadTest val = do return val

eitherTester :: [Int] -> Either String Int
{-Turns out return "!!" does not work; by default return binds to Right.-}
eitherTester []  = Left "!!"
eitherTester [v] = Left $ show v
eitherTester l   = return $ foldr1 (+) l

type Eitherer = Either String Int

mapMer :: (Int -> Int -> Int) -> [Int] -> Eitherer
mapMer op params = mapM unpack params >>= return . (foldr1 op)

unpack :: Int -> Eitherer
unpack n = return n
