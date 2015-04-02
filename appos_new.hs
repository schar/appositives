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
lambda :: MonadState S m => m a -> E -> m a
lambda m x = modify (++[x]) >> m

push :: MonadState S m => m E -> m E
push m = do
         x <- m
         modify (++[x]) >> return x -- notice that push's functionality
                                    -- is fully recoverable from lambda

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

newComma :: MonadWriter T m => (E -> m T) -> E -> m E
newComma f x = do { p <- f x; tell p; return x }

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

indef :: (E -> T) -> ST [] E
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

-- an odd, which l.t. [its successor, which g.t. it]
-- is odd. this is much smoother! niiiiiiiiiiiiiice.
stacked1,stacked2 :: TP
stacked1 = do
  o <- lift $ indef odd
  x <- newComma (lambda nrr1) o
  return $ odd x
  where nrr1 = do
               y <- lift $ pro 0
               z <- newComma (lambda nrr2) $ succ y
               return $ y < z
        nrr2 = do
               y <- lift $ pro 0
               z <- lift $ pro 1
               return $ y > z
-- [an odd, which l.t. its succ], which l.t. it, is odd
stacked2 = do
  o <- lift $ indef odd
  x <- newComma (lambda nrr1) o
  y <- newComma (lambda nrr2) x
  return $ odd y
  where nrr1 = lift $ do
                      x <- pro 0
                      y <- push . return $ succ x
                      return $ x < y
        nrr2 = lift $ do
                      x <- pro 0
                      y <- pro 1
                      return $ x < y

-- disjunction facts: can supplement a
-- disjunction, but cannot directly disjoin
-- supplemented things (the effect is to give
-- each of the supplements scope over the
-- disjunction). happily, this just follows
-- from the type theory. other accounts?
disj :: ST [] a -> ST [] a -> ST [] a
disj = mplus

-- as noted, it *is* possible for a disjunction
-- (though not a disjunct) to be supplemented.
-- the cases are not the smoothest, but i'd
-- expect this has to do with disjunctions not
-- being particularly good antecedents for
-- discourse anaphora in the first place.
twoOrThree :: ST [] E
twoOrThree = return 2 `disj` return 3

-- two or three, which is odd
twoOrThreeWhichOdd :: EP
twoOrThreeWhichOdd = do { x <- lift twoOrThree; newComma (lambda rel) x }
  where rel = do { x <- pro 0; return $ odd x }

-- an even, which l.t. its successor, l.t. it
bindInOut :: TP
bindInOut = do
  x <- lift $ indef even
  y <- newComma (lambda nrr) x
  z <- pro 0
  return $ y < z
  where nrr = lift $ do
                     y <- pro 0
                     u <- push . return $ succ y
                     return $ y < u

-- IO
display :: WriterT T (StateT S []) a -> [((a, T), S)]
display (WriterT (StateT f)) = f []

main :: IO ()
main = putStrLn "DO STHG"
