{-|
Module      : Control.Concurrent.Concurrential
Description : Description of concurrent computation with sequential components. 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE GADTs #-}

module Control.Concurrent.Concurrential (

    Concurrential

  , runConcurrential_
  , runConcurrential

  , embedIO
  , embedIOSequential
  , embedIOConcurrent

  ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent.Async

-- | Description of computation which is composed of sequential and concurrent
--   parts.
data Concurrential t where
  SCAtom
    :: IO s
    -- ^ Sequential part.
    -> (s -> IO t)
    -- ^ Concurrent part; fire off and then continue.
    -> Concurrential t
  SCBind :: Concurrential s -> (s -> Concurrential t) -> Concurrential t
  SCAp :: Concurrential (r -> t) -> Concurrential r -> Concurrential t

-- | It's essential that we have
--     Concurrential t -> IO (Async t)
--   rather than just
--     Concurrential t -> IO t
--   because the former allows us to handle the SCAp case in a straightforward
--   recusrive way, as seen below.
--
--   All sequential parts will indeed be run in sequence, as though they were
--   in the usual IO monad. The concurrent parts will be run in separate
--   threads after their associated sequential parts (the one it's paired with
--   in SCAtom).
runConcurrential_ :: Concurrential t -> IO (Async t)
runConcurrential_ sc = case sc of
    -- For SCAtom we do the sequential work, and then spark a thread for the
    -- concurrent work, returning its Async but not waiting for it. This
    -- allows concurrency to arise.
    SCAtom ss c -> do
        s <- ss
        asyncS <- async $ c s
        return asyncS
    -- For SCBind we cannot leverage any concurrency.
    SCBind sc next -> do
        asyncS <- runConcurrential_ sc
        s <- wait asyncS
        runConcurrential_ $ next s
    SCAp left right -> do
        asyncF <- runConcurrential_ left
        asyncX <- runConcurrential_ right
        -- At this point the sequential work for left and right has been
        -- accomplished. Now we have two asyncs, and we must combine them into
        -- one.
        let waitAndApply = do
              f <- wait asyncF
              x <- wait asyncX
              return $ f x
        asyncFX <- async waitAndApply
        return asyncFX

runConcurrential :: Concurrential t -> IO t
runConcurrential = (=<<) wait . runConcurrential_

instance Functor Concurrential where
  fmap f sc = case sc of
    SCAtom s c -> SCAtom s ((fmap . fmap) f c)
    SCBind sc k -> SCBind sc ((fmap . fmap) f k)
    SCAp sf sx -> SCAp ((fmap . fmap) f sf) sx

instance Applicative Concurrential where
  pure x = SCAtom (return x) return
  (<*>) = SCAp

instance Monad Concurrential where
  return = pure
  (>>=) = SCBind

embedIOSequential :: IO t -> Concurrential t
embedIOSequential io = SCAtom io return

embedIOConcurrent :: IO t -> Concurrential t
embedIOConcurrent io = SCAtom (return ()) (const io)

embedIO :: IO s -> (s -> IO t) -> Concurrential t
embedIO = SCAtom
