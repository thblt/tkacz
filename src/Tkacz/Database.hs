module Tkacz.Database
  (Database,
    open,
    close) where

import Data.HashMap
import System.FilePath

data Void = Void

data Database = Database {
  path :: FilePath,
  context :: Maybe Void,
  objects :: Map String Void,
  categories :: [Void] }

open :: FilePath -> IO (Either String Database)
open = undefined

close :: Database -> IO ()
close = undefined
