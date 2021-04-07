module Calcit.Bundle where

import Prelude
import ArgParse.Basic (ArgParser, argument, default, flagHelp, fromRecord, parseArgs, printArgError)
import Cirru.Edn (CirruEdn(..), writeCirruEdn)
import Cirru.Node (CirruNode(..))
import Cirru.Parser (parseCirru)
import Control.Parallel (parTraverse)
import Data.Array (zip, (!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import Node.Buffer as Buffer
import Node.ChildProcess (defaultExecOptions)
import Node.ChildProcess as ChildProcess
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Process (argv)

fromToParser :: ArgParser { from :: String, to :: String }
fromToParser =
  flagHelp
    *> fromRecord
        { from: default "src" $ argument [ "--from", "-f" ] "from folder"
        , to: default "bundled.cirru" (argument [ "--to", "-t" ] "to file name")
        }

main :: Effect Unit
main = do
  cliArgs <- (Array.drop 2) <$> argv
  let
    options = parseArgs "pure-run-bundle" "bundler of calcit files" fromToParser cliArgs
  fromTo <- case options of
    Left a -> do
      log $ printArgError a
      throw $ "parsed nothing from " <> (show cliArgs) <> ".. "
    Right o -> pure o
  launchAff_ do
    -- fromTo <- liftEffect detectsFromTo
    result <- execAff $ "find " <> fromTo.from <> " | grep -e '\\.cirru'"
    let
      files = Array.filter (not <<< String.null) (parseFilepaths result)
    if Array.length files > 0 then do
      -- liftEffect $ log $ "files: " <> show files
      contents <- parTraverse (readTextFile UTF8) files
      ednData <- liftEffect (buildEdn (zip files contents))
      writeTextFile UTF8 fromTo.to (writeCirruEdn ednData) -- TODO format into Cirru
      liftEffect $ log $ "wrote to " <> fromTo.to <> ". please check!"
    else
      liftEffect $ throw "found no cirru files"

buildEdn :: Array (Tuple String String) -> Effect CirruEdn
buildEdn xs = do
  ys <- traverse turnFileIntoData (map snd xs)
  let
    tryPkg = guessPkg (map fst ys)
  let
    configs =
      CrEdnMap
        ( Map.fromFoldable
            [ (Tuple (CrEdnKeyword "init-fn") (CrEdnString (tryPkg <> ".main/main")))
            , (Tuple (CrEdnKeyword "reload-fn") (CrEdnString "TODO"))
            , (Tuple (CrEdnKeyword "version") (CrEdnString "0.0.0"))
            , (Tuple (CrEdnKeyword "modules") (CrEdnList []))
            ]
        )
  pure
    ( CrEdnMap
        ( Map.fromFoldable
            [ (Tuple (CrEdnKeyword "package") (CrEdnString tryPkg))
            , (Tuple (CrEdnKeyword "configs") configs)
            , (Tuple (CrEdnKeyword "files") (CrEdnMap (Map.fromFoldable ys)))
            ]
        )
    )
  where
  guessPkg :: Array CirruEdn -> String
  guessPkg ys = case ys !! 0 of
    Just (CrEdnString ns) -> case (String.split (Pattern ".") ns) !! 0 of
      Just s -> s
      Nothing -> ns
    Just _ -> "TODO"
    Nothing -> "TODO"

turnFileIntoData :: String -> Effect (Tuple CirruEdn CirruEdn)
turnFileIntoData s = case parseCirru s of
  CirruLeaf _ -> throw "expected list"
  CirruList xs -> case xs !! 0 of
    Just (CirruList ys) -> case ys !! 1 of
      Just (CirruLeaf ns) -> do
        defs <- turnDefs (Array.drop 1 xs)
        let
          content =
            ( CrEdnMap
                ( Map.fromFoldable
                    [ (Tuple (CrEdnKeyword "ns") (CrEdnQuote (CirruList ys)))
                    , (Tuple (CrEdnKeyword "defs") (CrEdnMap defs))
                    ]
                )
            )
        pure (Tuple (CrEdnString ns) content)
      Just _ -> throw "expected ns to be a string"
      Nothing -> throw "missing ns field"
    Just a -> throw "expected ns part in a list"
    Nothing -> throw "expected ns part at 0"

turnDefs :: Array CirruNode -> Effect (Map CirruEdn CirruEdn)
turnDefs xs = do
  ys <-
    traverse
      ( \x -> case x of
          CirruLeaf _ -> throw "expected def in list"
          CirruList zs -> case zs !! 1 of
            Just (CirruLeaf name) -> pure (Tuple (CrEdnString name) (CrEdnQuote (CirruList zs)))
            Just (CirruList a) -> throw $ "unknown syntax on dep: " <> (show a)
            Nothing -> throw "missing definition of def"
      )
      xs
  pure (Map.fromFoldable ys)

execAff :: String -> Aff String
execAff command =
  makeAff
    $ \resolve ->
        ChildProcess.exec command defaultExecOptions
          ( \results -> do
              text <- Buffer.toString UTF8 results.stdout
              resolve (Right text) -- TODO error not handled
          )
          $> nonCanceler

parseFilepaths :: String -> Array String
parseFilepaths s = split (Pattern "\n") (trim s)
