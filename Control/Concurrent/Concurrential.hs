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

  , runConcurrential

  , sequentially
  , concurrently

  ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent.Async hiding (concurrently)

data Choice t = Sequential (IO t) | Concurrent (IO t)

instance Functor Choice where
  fmap f choice = case choice of
      Sequential io -> Sequential $ fmap f io
      Concurrent io -> Concurrent $ fmap f io

-- | Description of computation which is composed of sequential and concurrent
--   parts.
data Concurrential t where
  SCAtom :: Choice t -> Concurrential t
  SCBind :: Concurrential s -> (s -> Concurrential t) -> Concurrential t
  SCAp :: Concurrential (r -> t) -> Concurrential r -> Concurrential t

instance Functor Concurrential where
  fmap f sc = case sc of
    SCAtom choice -> SCAtom $ fmap f choice
    SCBind sc k -> SCBind sc ((fmap . fmap) f k)
    SCAp sf sx -> SCAp ((fmap . fmap) f sf) sx

instance Applicative Concurrential where
  pure = SCAtom . Sequential . pure
  (<*>) = SCAp

instance Monad Concurrential where
  return = pure
  (>>=) = SCBind

runConcurrentialK :: Concurrential t -> (Async t -> IO r) -> IO r
runConcurrentialK sc k = case sc of
    SCAtom choice -> case choice of
        Sequential io -> io >>= \x -> withAsync (return x) k
        Concurrent io -> withAsync io k
    SCBind sc next -> runConcurrentialK sc $ \asyncS -> do
        s <- wait asyncS
        runConcurrentialK (next s) k
    SCAp left right ->
        runConcurrentialK left $ \asyncF ->
        runConcurrentialK right $ \asyncX ->
        let waitAndApply = do
              f <- wait asyncF
              x <- wait asyncX
              return $ f x
        in withAsync waitAndApply k

runConcurrential :: Concurrential t -> IO t
runConcurrential c = runConcurrentialK c wait

sequentially :: IO t -> Concurrential t
sequentially = SCAtom . Sequential

concurrently :: IO t -> Concurrential t
concurrently = SCAtom . Concurrent

