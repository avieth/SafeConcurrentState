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

{-
data Concurrential t where
  Concurrential :: IO s -> (s -> IO t) -> Concurrential t

instance Functor Concurrential where
  fmap f (Concurrential s c) = Concurrential s ((fmap . fmap) f c)

instance Applicative Concurrential where
  pure x = Concurrential (pure x) pure
  -- ^ We produce x without concurrency!
  (Concurrential s cf) <*> (Concurrential s' cx) = Concurrential
-}

-- Ok so it's not so simple after all; we don't want to run every sequential
-- part, but instead to express the interleaving of sequential and concurrent
-- parts: work, fork, work, fork, work, fork, etc.
-- So the definition given here is no good.
-- I think what we really need is just a newtype over IO!
-- No, that's no good, it loses the distinction between a sequential part and
-- a concurrent part. How about something like this:
--
--   [(IO a, a -> IO b)]
--
-- where the a, b can vary throughout the list? How to express this?
-- Remember: when we bind, we _will_ have to wait for the concurrent part.
-- But when we <*> ... well can't we just design the datatype to accomodate
-- that? That's where the list comes in. Is there any point to even have a
-- concurrent component in the case of a bind? I don't think so. No, that's the
-- whole point of using the applicative combinator: the bind gives you no
-- opportunity for concurrency because you need the result of the computation
-- before continuing. So, what we want is a list of ... nah, we just describe
-- exactly what pure, >>=, and <*> do, syntactically! The AP case will be a
-- little involved, though, since it's there that we must express the special
-- property which we chase.
--

data Concurrential t where
  SCAtom :: IO s -> (s -> IO t) -> Concurrential t
  SCBind :: Concurrential s -> (s -> Concurrential t) -> Concurrential t
  SCAp :: Concurrential (r -> t) -> Concurrential r -> Concurrential t

-- | It's essential that we have
--     Concurrential t -> IO (Async t)
--   rather than just
--     Concurrential t -> IO t
--   because the former allows us to implement runConcurrential for SCAp
--   in a straightforward recusrive way.
runConcurrential_ :: Concurrential t -> IO (Async t)
runConcurrential_ sc = case sc of
    SCAtom ss c -> do
        s <- ss
        asyncS <- async $ c s
        return asyncS
    SCBind sc next -> do
        asyncS <- runConcurrential_ sc
        s <- wait asyncS
        runConcurrential_ $ next s
    SCAp left right -> do
        asyncF <- runConcurrential_ left
        asyncX <- runConcurrential_ right
        -- Now we have two asyncs, and we must combine them into one.
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
