{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Tkacz
Description : The strange knowledge manager.
Copyright   : © 2021 Thibault Polge
License     : GPL-3
Maintainer  : thibault@thb.lt
Stability   : experimental
-}

module Tkacz where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Coerce
import           Data.Default
import qualified Data.Map as M
import qualified Data.Tree as T

data Object = Object String

newtype ObjectId = ObjectId Int
  deriving (Eq, Ord, Show)

-- * Repositories

-- | A complete Tkacz repository.
--
-- Repositories are the combination of a collection of objects along
-- with a taxonomy, that can hold multiple concurrent classifications
-- of these objects.
data Repository = Repository
  {
    rObjects :: M.Map ObjectId Object
  , rClassification :: Taxonomy ObjectId
  }

-- | The Empty repository
empty :: Repository
empty = Repository M.empty (Taxonomy $ makeId 0)

-- | Insert a new object inside a repository.
insert :: Object -> Repository -> Repository
insert = undefined

-- | Get an object from the repository.
get :: ObjectId -> Repository -> Extracted Object
get = undefined

-- | Update
update :: Extracted Object -> Repository -> Repository
update = undefined

-- | Make an ObjectId from an integer.
makeId :: Int -> ObjectId
makeId = coerce

-- ** Extracted object

-- | An object extracted from a repository.
data Extracted o = Extracted ObjectId o

instance Functor Extracted where
  fmap f (Extracted id o) = Extracted id (f o)

replace :: o -> Extracted o -> Extracted o
replace o = fmap $ const o

unwrap :: Extracted o -> o
unwrap (Extracted _ o) = o

-- * Taxonomies

-- | A Taxonm
data Taxonomy a = Taxonomy a

type TaxonId = Int

data TaxonDefinition =
  -- | Children only
  EmptyTaxon
  -- | Manually-populated node
  | ManualTaxon
  -- | A Taxon that generates subnodes that split its contents.
  | BreakdownTaxon

-- * Entities

-- ** References

-- | A Dereferencable d holds an object of type o it can dereference
-- from a repository r.
class Dereferencable d r o | d -> o  where
  deref' :: d -> r -> d
  deref :: d -> r -> Maybe o
  deref = derefObject . deref'

-- | A Ref references an object of O by type i.
data Ref i o = Ref i
             | Deref i (Maybe o)


instance Dereferencable (Ref ObjectId Object) Repository  where
  refObject (Ref _) = Nothing
  refObject (Deref _ o) = o

-- | Like SimpleRef, but with an extra field for annotation.
data TzRef a =
  TzRef  { refAnnotation :: a
         , refRef :: Ref ObjectId Object }

--instance Dereferencable AnnotatedRef Repository Object where
--  deref = undefined

data BasicTitle = BasicTitle
  { preTitle :: Maybe String
  , title :: String
  , subTitle :: Maybe String }

data Book = Book {
  bookTitle :: BasicTitle
  , bookAuthors :: [TzRef () ObjectId Object] }

-- * The Tkacz Monad

type Output = [String]

data Config = Config {programName :: String}

instance Default Config where
  def = Config "Tkacz!"

-- | Tkacz is a config Reader, an Output Writer for logging info
type Tkacz = ReaderT Config (WriterT Output (StateT Repository IO))

tzInit :: Tkacz ()
tzInit = do
  liftIO $ putStrLn "Hello!"
