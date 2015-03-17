-- cool but idle :(
-- not so importantly lazy nemore :( :(
-- myPair ::  Num a => [a] -> ((a, [a]), (a, [a]))
-- myPair s = ((last . snd . snd $ myPair s, s ++ [1]),
--             (last . snd . fst $ myPair s, s ++ [2]))

-- State monad
type E = Int
type Stack = [E]
type MS a = Stack -> (a, Stack)

unitS ::  a -> MS a
unitS a s = (a, s)

bindS ::  MS a -> (a -> MS b) -> MS b
bindS m k s = k a s'
  where (a, s') = m s

-- WriterT State monad
-- Monoid : <{T, F}, &&, T>
type M a = MS (a, Bool)

unit ::  a -> M a
unit a = unitS (a, True)

bind ::  M a -> (a -> M b) -> M b
bind m k =
  m `bindS` \(a, w) ->
    k a `bindS` \(b, nw) ->
      unitS (b, w && nw)

lift ::  MS a -> M a
lift m = m `bindS` unit   -- ergo, dispensable

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

-- Tick and pro
tickK ::  E -> MS E
tickK x s = unitS x $ s ++ [x]

tick ::  MS E -> MS E
tick m = m `bindS` tickK   -- can be dispensed with.. but
                           -- kept around for convenience

pro ::  MS E
pro s = unitS (last s) s

-- Comma
comma ::  (E -> MS Bool) -> (E -> M E)
comma k x =
  k x `bindS` \w ->
     unitS (x, w)

-- Sanity check
extract ::  Show a => M a -> String
extract m = show $ m []

k0 ::  E -> M E
k0 = comma $ unitS . even  -- which is even [NRC]

subj ::  M E
subj = (tick $ unitS 3) `bindS` k0  -- 3, which is even

sent :: M Bool
sent =
  subj `lapply`
    (unit (==) `rapply` lift pro)   -- 3, which is even, equals itself
                                    -- mainly True, False on the side,
                                    -- binding succeeds!

main ::  IO ()
main = do
  putStrLn . extract $ sent

-- TODO:
-- test nested ARCs
-- test quantifiers and how appositives attach
-- treat nominal appositives
-- figure out which combinators necessary
