{-|
Description : Classification support

= Introduction

Tkacz organizes objects in __taxonomies__ and __contexts__.  Contexts
are top-level classifiers: most of the work done in Tkacz is done
inside of a context.  Internally, they're nothing more than taxons
with a special semantics.

Taxon can be of three types:

 - Standard taxons (@ManualTaxon@) contains objects added manually by
   the user.  They may be clades
-}

module Tkacz.Taxonomy
  () where

type Query = String

data Taxon =
  Taxon {
  taxonId :: String,
  taxonName :: String,
  taxonType :: TaxonType,
  taxonChildren :: TaxonChildren
  } deriving (Eq, Read, Show)

data TaxonType =
  ManualTaxon {
  taxonContext :: Bool
  }
  | QueryTaxon Query
  | BreakdownTaxon
  deriving (Eq, Read, Show)

data TaxonChildren = Clade | Children [Taxon]
  deriving (Eq, Read, Show)
