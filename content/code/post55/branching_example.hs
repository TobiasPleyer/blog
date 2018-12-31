#!/usr/bin/env stack
{- stack script
   --resolver lts-11.8
   --package base
   --package process
   --package transformers
-}

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.Lazy
import Control.Monad (liftM)
import System.Exit
import System.IO
import System.Process hiding (shell)


newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }
instance Monad m => Functor (EitherT e m) where
  fmap f = EitherT . liftM (fmap f) . runEitherT
  {-# INLINE fmap #-}
instance Monad m => Applicative (EitherT e m) where
  pure a  = EitherT $ return (Right a)
  {-# INLINE pure #-}
  EitherT f <*> EitherT v = EitherT $ f >>= \mf -> case mf of
    Left  e -> return (Left e)
    Right k -> v >>= \mv -> case mv of
      Left  e -> return (Left e)
      Right x -> return (Right (k x))
  {-# INLINE (<*>) #-}
instance Monad m => Monad (EitherT e m) where
  return a = EitherT $ return (Right a)
  {-# INLINE return #-}
  m >>= k  = EitherT $ do
    a <- runEitherT m
    case a of
      Left  l -> return (Left l)
      Right r -> runEitherT (k r)
  {-# INLINE (>>=) #-}
  fail = EitherT . fail
  {-# INLINE fail #-}
instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right
  {-# INLINE lift #-}
instance MonadIO m => MonadIO (EitherT e m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}
-- | Analogous to 'Left'. Equivalent to 'throwError'.
left :: Monad m => e -> EitherT e m a
left = EitherT . return . Left
{-# INLINE left #-}

-- | Analogous to 'Right'. Equivalent to 'return'.
right :: Monad m => a -> EitherT e m a
right = return
{-# INLINE right #-}


type MyStack a = EitherT String (WriterT [String] IO) a


shell :: String -> MyStack String
shell cmd = do
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode "/bin/bash" ["-c", cmd] []
  if (exitCode == ExitSuccess)
  then EitherT $ writer (Right (stdout), [stdout])
  else EitherT $ writer (Left (stdout), [stdout, stderr])

main = do
  (res,logs) <- runWriterT $ runEitherT $ do
    shell "echo 'Good'; exit 0"
    shell "echo 'Bad' >&2; exit 1"
    shell "echo 'Good'; exit 0"
  case res of
    Right _ -> putStrLn "SUCCESS"
    Left _ -> putStrLn "FAILURE"
  print res
  putStrLn "== INFO =="
  mapM_ putStrLn logs
