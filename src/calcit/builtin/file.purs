module Calcit.Builtin.File where

import Calcit.Primes (CalcitData(..))
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Prelude (bind, pure, ($), (<>), show, discard)

-- | reading file from working directory
fnNativeReadFile :: (Array CalcitData) -> Effect CalcitData
fnNativeReadFile xs = case xs !! 0 of
  Just (CalcitString s) -> do
    content <- readTextFile UTF8 s
    pure $ CalcitString content
  Just a -> throw $ "read-file expected string, got: " <> (show a)
  Nothing -> throw "read-file expected 1 argument"

fnNativeWriteFile :: (Array CalcitData) -> Effect CalcitData
fnNativeWriteFile xs = case xs !! 0, xs !! 1 of
  Just (CalcitString s), Just (CalcitString content) -> do
    writeTextFile UTF8 s content
    pure CalcitNil
  Just a, Just b -> throw $ "write-file expected 2 strings, got: " <> (show a) <> " " <> (show b)
  _, _ -> throw "write-file expected 2 arguments"
