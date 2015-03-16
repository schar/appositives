-- cool but idle :(
-- not so importantly lazy nemore :( :(
-- myPair ::  Num a => [a] -> ((a, [a]), (a, [a]))
-- myPair s = ((last . snd . snd $ myPair s, s ++ [1]),
--             (last . snd . fst $ myPair s, s ++ [2]))

-- State monad
type Stack = [Int]
type MS a = Stack -> (a, Stack)

unitS ::  a -> MS a
unitS a s = (a, s)

bindS ::  MS a -> (a -> MS b) -> MS b
bindS m k s = k a s'
  where (a, s') = m s

-- WriterT State monad
type M a = Stack -> ((a, Bool), Stack)

unit ::  a -> M a
unit a = unitS (a, True)

bind ::  M a -> (a -> M b) -> M b
bind m k s = ((b, p && q), s'')
  where
    ((a, p), s' ) = m s
    ((b, q), s'') = k a s'  -- should generalize

lift ::  MS a -> M a
lift m s = ((a, True), s')
  where (a, s') = m s

-- Applicative functors
rapply ::  M (a -> b) -> M a -> M b
rapply m n =
  m `bind` \f ->
    n `bind` \x ->
      unit $ f x

lapply ::  M a -> M (a -> b) -> M b
lapply m n =
  m `bind` \x ->
    n `bind` \f ->
      unit $ f x

-- Ticks
tick ::  MS Int -> MS Int
tick m =
  m `bindS` \x -> \s ->
    unitS x $ s ++ [x]  -- should maybe just be defined
                        -- in terms of State monad

-- Comma
comma ::  M Bool -> M (a -> a)
comma m =
  m `bind` \a -> \s ->
    ((\x -> x, a), s)

-- Lexicon
eq ::  Eq a => a -> a -> Bool
eq = (==)

gt ::  Ord a => a -> a -> Bool
gt = (>)

lt ::  Ord a => a -> a -> Bool
lt = (<)

neg ::  MS (Bool -> Bool)
neg = unitS not

-- Sanity check
extract ::  Show a => M a -> String
extract m = show $ m []

eq0 ::  M (Int -> Bool)
eq0 = lift $ \s -> (\x -> x == last s, s)

testCase ::  M Bool
testCase =
  ((lift . tick $ unitS 2)
    `lapply`
      (comma . lift $ \s -> (last s == 3, s)))
  `lapply`
  eq0

main ::  IO ()
main = do
  putStrLn . extract $ testCase