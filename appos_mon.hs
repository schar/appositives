import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
--import           System.Environment

data List a = List { runList :: [a] }

instance Monad List where
  return x     = List [x]
  List m >>= k = List $ concatMap (runList . k) m

instance Functor List where
  fmap f (List m) = List $ map f m

instance Applicative List where
  pure  = return
  (<*>) = ap

type E = Int
type T = Bool
type S = [E]

data StateT m a = StateT { runStateT :: S -> m (a, S)}

instance Monad m => Monad (StateT m) where
  return x       = StateT $ \s -> return (x, s)
  StateT m >>= k = StateT $ \s -> do
                                  (x, s') <- m s
                                  runStateT (k x) s'

instance MonadTrans StateT where
  lift m = StateT $ \s -> do
                          x <- m
                          return (x, s)

instance Monad m => Functor (StateT m) where
  fmap f (StateT m) = StateT $ \s -> do
                                     (x, s') <- m s
                                     return (f x, s')

instance Monad m => Applicative (StateT m) where
  pure  = return
  (<*>) = ap

data WriterT m a = WriterT { runWriterT :: m (a, T) }

instance Monad m  => Monad (WriterT m) where
  return x        = WriterT . return $ (x, True)
  WriterT m >>= k = WriterT $ do
                              (x, p) <- m
                              (y, q) <- runWriterT $ k x
                              return (y, p && q)

instance MonadTrans WriterT where
  lift m = WriterT $ do
                     x <- m
                     return (x, True)

instance Monad m => Functor (WriterT m) where
  fmap f (WriterT m) = WriterT $ do
                                 (x, p) <- m
                                 return (f x, p)

instance Monad m => Applicative (WriterT m) where
  pure  = return
  (<*>) = ap

--anaphora
push :: Monad m => StateT m E -> StateT m E
push m = do
         x <- m
         StateT $ \s -> runStateT (return x) (s++[x])

pro :: Monad m => Int -> StateT m E
pro n = StateT $ \s -> return (reverse s !! n, s)

--inj :: E -> WriterT (StateT List) E
--inj x = lift . push $ return x

-- NRCs
type P  = StateT List T
type EP = WriterT (StateT List) E
type TP = WriterT (StateT List) T

comma :: (E -> P) -> E -> EP
comma f x = WriterT $ do
                      p <- f x
                      return (x, p)

--rcOdd :: E -> P
--rcOdd x = return $ odd x

--threeWhichIsOdd :: EP
--threeWhichIsOdd = do
--                  x <- inj 3
--                  comma rcOdd x

rcLTSucc :: E -> P
rcLTSucc x = do
             y <- pro 0
             z <- push . return $ succ y
             return $ (<) x z

--threeWhichLTSucc :: EP
--threeWhichLTSucc = do
--                   x <- inj 3
--                   comma rcLTSucc x

-- indefinites

domain :: S
domain = [1..10]

indef :: (E -> T) -> StateT List E
indef prop = StateT $ \s ->
  List . concat $
    map (\x -> [(x, s)]) $
      filter prop domain

anOddWhichLTSucc :: EP
anOddWhichLTSucc = do
                   x <- (lift . push . indef) odd
                   comma rcLTSucc x

-- negation
neg :: P -> P
neg (StateT p) = StateT $ \s -> m s
  where filt s = filter (\(a, _) -> a) $ runList (p s)
        m s    = case (filt s) of
          [] -> List [(True , s)]
          _  -> List [(False, s)]


-- testing
--
-- binding into and out of NRC
-- an odd, which is less than its successor, is less than it
--
check :: TP
check = return (<) <*> anOddWhichLTSucc <*> lift (pro 0)

-- negation is defined in the "lexical"[?] monad (StateT
-- List). its type does not, it follows, let it combine
-- directly with anything that has supplemental content
-- instead, the latter must scope over the negation,
-- which allows us to form a P, which is then lifted into
-- the "outer" monad, like so...
--
try :: TP
try = do -- an odd, which is less than its succ, isn't odd
  x <- anOddWhichLTSucc -- see below for definition
  p <- lift . neg . return $ odd x
  return p

-- text sequencing, just monadized boolean conjunction
--
text1, text2 :: TP
text1 = return (&&) <*> check <*> lift (do { x <- pro 0; return $ odd x })
text2 = return (&&) <*> check <*> lift (do { x <- pro 1; return $ odd x })

-- stacked appositives
-- three, which l.t. its successor, which g.t. it, is odd
stacked :: TP
stacked = do
  x <- lift . push . return $ 3
  y <- comma rcLTSucc x
  z <- comma (\u -> do {v <- pro 0; return $ (<) u v }) y
  return $ odd z -- something i do not quite understand
                 -- is happening here with the order of
                 -- drefs, but broadly the anaphora and
                 -- stacking all works out... o_o

display :: WriterT (StateT List) a -> [((a, T), S)]
display (WriterT (StateT f)) = runList $ f []

main :: IO ()
main = putStrLn "DO STHG"
--(writeFile "x.txt" . show . display) check
