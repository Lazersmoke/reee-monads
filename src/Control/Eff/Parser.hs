{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Eff.Parser
  (Parser
  ,Parseable
  ,parseOne
  ,runParser
  ) where

import Control.Eff

class Parseable s where
  type ParseUnit s :: *
  uncons :: s -> Maybe (ParseUnit s,s)

instance Parseable [a] where
  type ParseUnit [a] = a
  uncons (x:xs) = Just (x,xs)
  uncons [] = Nothing

data Parser s a where
  ParseOne :: Parseable s => Parser s (ParseUnit s)

parseOne :: forall s r. (Parseable s, Member (Parser s) r) => Eff r (ParseUnit s)
parseOne = send (ParseOne @s)

runParser :: Parseable s => s -> Eff (Parser s ': r) a -> Eff r [(a,s)]
runParser = runParserCore

runParserCore :: Parseable s => s -> Eff (Parser s ': r) a -> Eff r [(a,s)]
runParserCore s (Pure x) = return [(x,s)]
runParserCore s (Eff u q) = case u of
  Inject ParseOne -> case uncons s of
    Just (x,xs) -> runParserCore xs (runTCQ q x)
    Nothing -> return []
  Weaken u' -> Eff u' (Singleton (runParserCore s . runTCQ q))
