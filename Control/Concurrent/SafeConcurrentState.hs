{-|
Module      : Control.Concurrent.SafeConcurrentState
Description : Safe impure computation with pure state.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

The SafeConcurrentState monad describes impure computation with concurrency
and state such that the state is always recoverable, even if an exception
is raised.

-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Control.Concurrent.SafeConcurrentState (

    SafeConcurrentState
  , sequentially
  , concurrently
  , get
  , set
  , modifySTM
  , modify
  , modify'
  , run
  , runBracket
  , runBracketRethrow

  ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Control.Concurrent.Concurrential as C
import Control.Exception
import Data.Typeable

-- | A Concurrential computation which depends upon a TVar holding state.
newtype SafeConcurrentState s a = SafeConcurrentState {
    runSafeConcurrentState :: TVar s -> C.Concurrential a
  } deriving (Typeable)

instance Functor (SafeConcurrentState s) where
  fmap f x = SafeConcurrentState $ \tvar -> fmap f (runSafeConcurrentState x tvar)

instance Applicative (SafeConcurrentState s) where
  pure x = SafeConcurrentState $ \_ -> pure x
  mf <*> mx = SafeConcurrentState $ \tvar ->
      runSafeConcurrentState mf tvar <*> runSafeConcurrentState mx tvar

instance Monad (SafeConcurrentState s) where
  return = pure
  m >>= k = SafeConcurrentState $ \tvar -> do
      x <- runSafeConcurrentState m tvar
      runSafeConcurrentState (k x) tvar

sequentially :: IO t -> SafeConcurrentState s t
sequentially action = SafeConcurrentState $ \_ -> C.sequentially action

concurrently :: IO t -> SafeConcurrentState s t
concurrently action = SafeConcurrentState $ \_ -> C.concurrently action

-- | Get the current image of the state.
get :: SafeConcurrentState s s
get = SafeConcurrentState $ \tvar -> C.concurrently . atomically $ readTVar tvar

-- | Set the state.
set :: s -> SafeConcurrentState s ()
set x = SafeConcurrentState $ \tvar -> C.concurrently . atomically $ writeTVar tvar x

-- | Atomically modify the state.
--   This is a wart. Should remove if possible.
modifySTM :: (s -> STM (a, s)) -> SafeConcurrentState s a
modifySTM k = SafeConcurrentState $ \tvar -> C.concurrently . atomically $ do
    var <- readTVar tvar
    (x, var') <- k var
    writeTVar tvar var'
    return x

modify :: (s -> (a, s)) -> SafeConcurrentState s a
modify f = modifySTM (return . f)

modify' :: (s -> s) -> SafeConcurrentState s ()
modify' f = modify (\x -> ((), f x))

-- | Run a safe concurrent computation. This will never throw an exception.
run :: s -> SafeConcurrentState s a -> IO (Either SomeException a, s)
run x action = do
    tvar <- newTVarIO x
    x <- (Right <$> (C.runConcurrential (runSafeConcurrentState action tvar)))
         `catch`
         (\(e :: SomeException) -> return (Left e))
    finalState <- atomically (readTVar tvar)
    return (x, finalState)

-- | Run a safe concurrent computation with continuations to be run in case of
--   an exception or the absence of an exception.
runBracket
  :: s
  -> SafeConcurrentState s a
  -> ((SomeException, s) -> IO b)
  -- ^ Run in case of exception.
  -> ((a, s) -> IO b)
  -- ^ Run in case of no exception.
  -> (b -> IO c)
  -- ^ Run no-matter-what.
  -> IO c
runBracket x action onException onNoException afterwards = do
    (outcome, state) <- run x action
    case outcome of
      Left exception -> onException (exception, state) >>= afterwards
      Right value -> onNoException (value, state) >>= afterwards

-- | Run a safe concurrent computation with continuations. This will throw
--   an exception if and only if the SafeConcurrentState computation threw
--   an exception, but the exception handler and finally action will be
--   run first.
runBracketRethrow
  :: s
  -> SafeConcurrentState s a
  -> ((SomeException, s) -> IO b)
  -> ((a, s) -> IO b)
  -> (b -> IO c)
  -> IO c
runBracketRethrow x action onException onNoException afterwards =
    runBracket x action onException' onNoException' afterwards'
  where
    -- Run the original onException but pass the exception through.
    onException' (e, s) = onException (e, s) >>= (\x -> return $ Left (e, x))
    -- Run the original onNoException, distinguishing it from the onException
    -- via Right
    onNoException' = (fmap . fmap) Right onNoException
    afterwards' = either (\(e, x) -> afterwards x >> throw e) afterwards
