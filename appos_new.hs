{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
                                       -- i don't really know what
                                       -- these do, but i know the
                                       -- compiler complains if i
                                       -- leave them out!

import           Control.Applicative
import           Control.Monad.List
import           Control.Monad.State
import           Control.Monad.Writer
                                       -- the monads we'll be relying
                                       -- on: State, List, Writer.
                                       -- Applicative gives us access
                                       -- to applicative functors.

-- basic types: our individuals are numbers. our foundational
-- monad is StateT. here, we leave open which monad m StateT
-- transforms, but in practice, m is always fixed to [] (i.e.
-- the List or [equivalently for our purposes] Set monad).
-- StateT [] is the analog of what I call the State.Set monad
-- in Charlow 2014.
type E = Int
type T = Bool
type S = [E]
type ST m a = StateT S m a

-- in order to log supplemental content, we'll wrap WriterT
-- around StateT []. the monoid of interest is [extensionally]
-- boolean values with boolean conjunction. this means that a
-- boolean value with some State, List, and Writer side effects
-- will look like \s1 [((t1, t2), s2) | ...] -- a minimal change
-- from the monadic booleans in Charlow 2014: \s1 [(t, s2) | ...].
instance Monoid Bool where
  mempty  = True
  mappend = (&&)

-- some apparatus for anaphora: push, λ, pro. λ is not strictly
-- necessary in the presence of push if we assemble relative
-- clauses along the lines of Shan & Barker 2006, or for that
-- matter Charlow 2014. conversely, push's functionality is
-- recoverable from λ. i'll use one or the other below depending
-- on which is more straightforwrad.
lambda :: MonadState S m => m a -> E -> m a
lambda m x = modify (++[x]) >> m

push :: MonadState S m => m E -> m E
push m = do
         x <- m
         modify (++[x]) >> return x

pro :: MonadState S m => Int -> m E
pro n = gets $ (!!n) . reverse -- indices count backwards from
                               -- the end of the stack. 0 is
                               -- 'most recent', 1 is next-most-
                               -- recent, etc.

-- here, i directly define some relative clauses, functions from
-- individuals into monadic booleans. rcLTSucc is the [restrictive]
-- relative clause "which is less than it_0's successor" [i.e. with
-- an unbound pronoun]. similarly for rcGTSucc. there are unbound
-- pronouns so that we can see how things can bind into non-
-- restrictive relatives later on. notice that using `push` means
-- the successor gets placed on the stack, as well. this will let
-- us see how we can bind out of non-restrictive relatives later.
type P  = ST [] T

rcLTSucc,rcGTSucc :: E -> P
rcLTSucc x = do
             y <- pro 0
             z <- push . return $ succ y
             return $ (<) x z
rcGTSucc x = do
             y <- pro 0
             z <- push . return $ succ y
             return $ (>) x z

-- a couple possible denotations for the COMMA (the thing that
-- toggles between at-issue and not-at-issue content). i think the
-- second of these is to be preferred since its more general type
-- allows a more straightforward account of stacked non-restrictive
-- relative clauses. `tell` in newComma is used for shunting an at-
-- issue proposition to the supplemental dimension.
type EP = WriterT T (StateT S []) E

comma :: Monad m => (E -> m T) -> E -> WriterT T m E
comma f x = WriterT $ do
                      p <- f x
                      return (x, p) -- DB: = \f -> listen . lift . f?

newComma :: MonadWriter T m => (E -> m T) -> E -> m E
newComma f x = do { p <- f x; tell p; return x }

-- indefinites -- as in Charlow 2014 (though i am assuming, for
-- simplicity, that the restrictor of an indefinite never harbors
-- side effects).
domain :: [E]
domain = [1..10]

indef :: (E -> T) -> ST [] E
indef prop = mfilter prop $ lift domain

-- an indefinite with a non-restrictive relative clause
-- i.e. ~ `an odd number, which is less than its successor`.
-- notice that the [nondeterministic] odd gets pushed onto
-- the stack. here i use comma, but newComma would work just
-- as well (we'd build up the RC, use return to coerce into
-- a WriterT boolean, then abstract over the "trace").
anOddWhichLTSucc :: EP
anOddWhichLTSucc = do
                   x <- (lift . push . indef) odd
                   comma rcLTSucc x

-- negation.. again as in Charlow 2014. demands failure,
-- throws away the side effects in its prejacent. notice
-- that negation is defined in the StateT monad. in general,
-- all lexical items will be restricted to such *at-issue
-- types*. this bears [i think] some resemblance to how potts
-- proceeds, as well as [perhaps] to barker, bernardi, shan.
neg :: P -> P
neg (StateT p) = StateT $ \s -> m s
  where filt s = filter (\(a, _) -> a) $ p s
        m s    = case (filt s) of
          [] -> [(True , s)]
          _  -> [(False, s)]

-- some test cases

-- binding into and out of NRC
--
-- an odd, which is less than its successor, is less than it
type TP = WriterT T (StateT S []) T

check1,check2 :: TP
check1 = return (<) <*> anOddWhichLTSucc <*> lift (pro 0)

-- an even[x] is l.t. 8, which is g.t. its[x] successor
check2 = return (<) <*> (lift . push . indef) even <*> obj
  where obj = do { x <- return 8; push $ comma rcGTSucc x }

-- negation is defined in the "lexical"[?] monad (StateT
-- List). its type does not, it follows, let it combine
-- directly with anything that has supplemental content.
-- instead, the latter must scope over the negation,
-- which allows us to form a P, which is then lifted into
-- the "outer" monad, like so...
--
-- an odd, which is less than its succ, isn't less than it
-- the at-issue content is false, the not-at-issue content
-- is true.
try :: TP
try = do
  x <- anOddWhichLTSucc
  lift . neg $ do { y <- pro 0; return $ x < y }

-- text sequencing, as ever just monadized boolean conjunction
text1, text2 :: TP
text1 = return (&&) <*> check1 <*> lift (do { x <- pro 0; return $ odd x })
text2 = return (&&) <*> check1 <*> lift (do { x <- pro 1; return $ odd x })

-- stacked NRCs. `an odd, which l.t. [its successor, which g.t.
-- it], is odd`. the use of newComma makes this go *much* smoother
-- than it could ever go with comma. niiiiiiiiiiiice.
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

-- another kind of stacking.
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

-- disjunction facts: can supplement a disjunction
-- (i.e. a or b, which c), but cannot directly disjoin
-- supplemented things (i.e. a, which c, or b, which d --
-- the effect here is to give each of the supplements
-- scope over the disjunction). happily, this follows
-- immediately from the type theory if we assume that
-- disjunction (like everything lexical) requires at-issue
-- types. what do other accounts predict for such cases?
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

-- one final example. binding into and out of
-- the non-restrictive relative. here we check
-- `an even, which l.t. its successor, l.t. it`.
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

-- IO: display evaluates a monadized individual or boolean
-- at the empty stack.
display :: WriterT T (StateT S []) a -> [((a, T), S)]
display (WriterT (StateT f)) = f []

main :: IO ()
main = putStrLn "DO STHG" -- why not?

-- some test cases:
-- display check1
-- display check2
-- display try
-- display text1
-- display text2
-- display stacked1
-- display stacked2
-- display twoOrThreeWhichOdd
-- display bindInOut
