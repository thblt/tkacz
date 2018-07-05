module Main (main) where

import Data.Default
import Data.List
import Data.Monoid
import Options.Applicative

import Data.Version (showVersion)

import qualified Tkacz

addOptions = many (flag' () (short 'v'))

data Syntax = Natural | JSON
  deriving (Eq, Read, Show)

data Invocation =
  Invocation {
  store :: FilePath,
  syntax :: Syntax,
  simulate :: Bool,
  what :: Command
  }

data Command = ObjectShow
             | ObjectSearch
             | ObjectEdit
             | ObjectNew
             | ObjectMerge
             | ObjectSplit

             | CatList
             | CatNew

             | ModelList
             | ModelDescribe

             | Shell
             | Version
  deriving (Eq, Read, Show)

ontologyOptions :: Parser Command
ontologyOptions = pure ObjectShow

alpha a b = a `alpha` b

parser :: Parser Invocation
parser = Invocation
  <$>
  option auto ( short 'C'
                <> help "Use the database at PATH"
                <> value "~/Documents/Tkacz"
                <> metavar "PATH")
                -- <> showDefault)
  <*>
  option auto ( long "syntax"
                <> help "Use SYNTAX"
                <> metavar "SYNTAX"
                <> value Natural
                <> showDefault)
  <*>
  switch ( long "simulate"
           <> short 'n'
           <> help "Don't actually make any changes to the database" )
  <*>
  (hsubparser
    (command "show" (info ontologyOptions ( progDesc "Display a given object" ))
      <> command "search" (info ontologyOptions ( progDesc "Search the database" ))
      <> command "locate" (info ontologyOptions ( progDesc "Locate a copy of an object" ))
      <> command "edit"  (info ontologyOptions ( progDesc "Change a given object" ))
      <> command "new" (info ontologyOptions ( progDesc "Create a new object" ))
      <> command "merge" (info ontologyOptions ( progDesc "Merge duplicate objects" ))
      <> command "split" (info ontologyOptions ( progDesc "Split an object in two or more" ))
      <> command "trash" (info ontologyOptions ( progDesc "Remove object" ))
      <> commandGroup "Object manipulation commands:"
  )
  <|>
  hsubparser
  (command "categories" (info ontologyOptions ( progDesc "List known categories" ))
    <> command "group" (info ontologyOptions ( progDesc "Group things into a category" ))
    <> command "ungroup" (info ontologyOptions ( progDesc "Remove things from a category" ))
    <> command "newcat" (info ontologyOptions ( progDesc "Make a new category" ))
    <> command "rmcat" (info ontologyOptions ( progDesc "Remove a category" ))
    <> commandGroup "Contexts and categories commands:"
    <> hidden)

  <|>

  hsubparser
  (command "log" (info ontologyOptions ( progDesc "Show revision history" ))
    <> command "undo" (info ontologyOptions ( progDesc "Undo an operation" ))
    <> commandGroup "History commands:"
    <> hidden)

  <|>

  hsubparser
  (command "types" (info ontologyOptions ( progDesc "List known entity types" ))
    <> command "describe" (info ontologyOptions ( progDesc "Display information about some entity" ))
    <> commandGroup "Ontology commands:"
    <> hidden)

  <|>

  hsubparser
  (command "shell" (info ontologyOptions ( progDesc "Run the Tkacz shell" ))
    <> command "version" (info ontologyOptions ( progDesc "Print version number and exit" ))
    <> commandGroup "Other commands:"
    <> hidden))

cmdVersion :: Invocation -> IO ()
cmdVersion _ = putStrLn $ "Tkacz version " ++ showVersion Tkacz.version

route :: Invocation -> IO ()
route i = let go' Version = cmdVersion
              go' ObjectShow = cmdVersion
          in
            go' (what i) i

main = do
  opts <- execParser (info (parser <**> helper) (progDesc Tkacz.description))
  route opts
