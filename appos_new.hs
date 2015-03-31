{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           Control.Applicative
import           Control.Monad.List
import           Control.Monad.State
import           Control.Monad.Writer

type E = Int
type T = Bool
type S = [E]
type ST m a = StateT S m a

instance Monoid Bool where
  mempty  = True
  mappend = (&&)

-- anaphora: push, λ, pro
push :: MonadState S m => m E -> m E
push m = do
         x <- m
         modify (++[x]) >> return x

lambda :: MonadState S m => m a -> E -> m a
lambda m x = modify (++[x]) >> m

pro :: MonadState S m => Int -> m E
pro n = gets $ (!!n) . reverse

-- NRCs
type P  = ST [] T
type EP = WriterT T (StateT S []) E
type TP = WriterT T (StateT S []) T

comma :: (E -> P) -> E -> EP
comma f x = WriterT $ do
                      p <- f x
                      return (x, p) -- DB: = \f -> listen . lift . f?

rcLTSucc,rcGTSucc :: E -> P
rcLTSucc x = do
             y <- pro 0
             z <- push . return $ succ y
             return $ (<) x z
rcGTSucc x = do
             y <- pro 0
             z <- push . return $ succ y
             return $ (>) x z

-- indefinites
domain :: S
domain = [1..10]

indef :: (E -> T) -> StateT S [] E
indef prop = mfilter prop $ lift domain

anOddWhichLTSucc :: EP
anOddWhichLTSucc = do
                   x <- (lift . push . indef) odd
                   comma rcLTSucc x

-- negation
neg :: P -> P
neg (StateT p) = StateT $ \s -> m s
  where filt s = filter (\(a, _) -> a) $ p s
        m s    = case (filt s) of
          [] -> [(True , s)]
          _  -> [(False, s)]

-- testing
--
-- binding into and out of NRC
-- an odd, which is less than its successor, is less than it
check :: TP
check = return (<) <*> anOddWhichLTSucc <*> lift (pro 0)
-- an even[x] is l.t. 8, which is g.t. its[x] successor
cheque :: TP
cheque = return (<) <*> (lift . push . indef) even <*> obj
  where obj = do { x <- return 8; push $ comma rcGTSucc x }

-- negation is defined in the "lexical"[?] monad (StateT
-- List). its type does not, it follows, let it combine
-- directly with anything that has supplemental content.
-- instead, the latter must scope over the negation,
-- which allows us to form a P, which is then lifted into
-- the "outer" monad, like so...
--
-- an odd, which is less than its succ, isn't odd
try :: TP
try = do
  x <- anOddWhichLTSucc
  lift . neg . return $ odd x

-- text sequencing, just monadized boolean conjunction
text1, text2 :: TP
text1 = return (&&) <*> check <*> lift (do { x <- pro 0; return $ odd x })
text2 = return (&&) <*> check <*> lift (do { x <- pro 1; return $ odd x })

-- stacked appositives
-- [three, which l.t. its successor], which l.t. it, is odd
stacked1,stacked2 :: TP
stacked1 = do
  x <- lift . push . return $ 3
  y <- comma rcLTSucc x
  z <- comma (\u -> do {v <- pro 0; return $ (<) u v }) y
  return $ odd z
-- three, which l.t. [its successor, which g.t. it], is odd
stacked2 = do
  x <- lift . push . return $ 3                            -- 3 [+ dref]
  y <- lift . push $ return succ <*> pro 0                 -- 3's succ [+ dref]
  z <- comma (\u -> do { v <- pro 1; return $ (>) u v }) y -- 3's succ, which > 3
  u <- comma (\v -> return $ (<) v z) x                    -- 3, which < its succ
  return $ odd u -- perhaps cleaner to separate the
                 -- 1D-to-2D piping step from the
                 -- toggling between the two monads..
                 -- that way, can assume that piping
                 -- always operates on functions into
                 -- WriterT, avoid odd scopings like
                 -- those in evidence in stacked2

display :: WriterT T (StateT S []) a -> [((a, T), S)]
display (WriterT (StateT f)) = f []

-- disjunction facts: can supplement a
-- disjunction, but cannot directly disjoin
-- supplemented things (the effect is to give
-- each of the supplements scope over the
-- disjunction). happily, this just follows
-- from the type theory. other accounts?
disj :: ST [] a -> ST [] a -> ST [] a
disj = mplus

-- as noted, it *is* possible for a disjunction
-- (though not a disjunct) to be supplemented:
twoOrThree :: ST [] E
twoOrThree = push $ return 2 `disj` return 3

-- two or three, which is odd
twoOrThreeWhichOdd :: EP
twoOrThreeWhichOdd = do { x <- lift twoOrThree; comma rel x }
  where rel x = return $ odd x

main :: IO ()
main = putStrLn "DO STHG"
