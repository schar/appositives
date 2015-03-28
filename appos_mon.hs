import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans

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
type S = [E]

data StateT m a = StateT { runStateT :: S -> m (a, S)}

instance Monad m => Monad (StateT m) where
  return x       = StateT $ \s -> return (x, s)
  StateT m >>= k = StateT $ \s -> do
                                  (x, s') <- m s
                                  runStateT (k x) s'

instance MonadTrans StateT where
  lift m = StateT $ \s -> do { x <- m; return (x, s) }

instance Monad m => Functor (StateT m) where
  fmap f (StateT m) = StateT $ \s -> do
                                     (x, s') <- m s
                                     return (f x, s')

instance Monad m => Applicative (StateT m) where
  pure  = return
  (<*>) = ap

data WriterT m a = WriterT { runWriterT :: m (a, Bool) }

instance Monad m  => Monad (WriterT m) where
  return x        = WriterT . return $ (x, True)
  WriterT m >>= k = WriterT $ do
                              (x, p) <- m
                              (y, q) <- runWriterT $ k x
                              return (y, p && q)

instance MonadTrans WriterT where
  lift m = WriterT $ do { x <- m; return (x, True) }

instance Monad m => Functor (WriterT m) where
  fmap f (WriterT m) = WriterT $ do
                                 (x, p) <- m
                                 return (f x, p)

instance Monad m => Applicative (WriterT m) where
  pure  = return
  (<*>) = ap

tick :: Monad m => StateT m E -> StateT m E
tick m = do { x <- m; StateT $ \s -> runStateT (return x) (s++[x]) }

inj :: Int -> WriterT (StateT List) Int
inj x = lift . tick $ return x

display :: WriterT (StateT List) a -> [((a, Bool), S)]
display (WriterT (StateT f)) = runList $ f []

add :: WriterT (StateT List) Int
add = return (+) <*> inj 3 <*> inj 2

main :: IO ()
main = print . display $ add
