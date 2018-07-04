{-|
Description: the Tkacz typesystem

= Introduction

Tkacz data model is not implemented directly with Haskell ADTs, but
using an intermediate typesytem which allows for runtime
introspection, true genericity with heterogeneity without requiring
tons of boilerplate.
-}

module Tkacz.Typesystem
  ( Ontology (..)
  , NamedType (..)
  , NamedTypeClass (..)
  , Instantiability (..)
  , Type (..)
  , Identifier
  , Value (..)) where

import Data.List (intercalate)
import Data.Map (Map (..))
import Data.Tree (Tree (..)
                 , Forest (..))
import qualified Tkacz.AddressableTree as TAT

-- | In Tkacz slang, an Ontology is a structured representation of
--   what things represented in Tkacz can be.  The ontology stored
--   *all and every* type available (hence its name).  Tkacz ontology
--   is hierarchical: classes inherit from one another.
--
-- The ontology is a Forest (and not a Tree) because only Entities
-- need share a common root: regular classes and relationship live in
-- their own trees.

type Ontology = Forest NamedType

-- | A 'NamedType' is the first manipulable type in a Tkacz system.
--   Unlike regular 'Type's, 'NamedType's have an optional parent, a
--   class and (obviously) a name.
data NamedType = NamedType
  { name :: String
  , parent :: Maybe String
  , entity :: NamedTypeClass
  , definition :: Type
  , doc :: String }

instance TAT.Named NamedType where
  name = name

{-| 'NamedType's can be three different things: entities, classes or
    relationships.

 - An 'Entity' is the type for the objects Tkacz represent.  Books,
   articles, persons, journals... are entities.  Entities have
   identity, are accessed through Relationships.

   Entity names must start with an uppercase letter.

 - A 'Class' is like an entity, except it lacks identity.  Title,
   publication date, etc, are classes: they're complex objects.

   Class names start with a lowercase letter.

 - A 'Relationship' associates an entity to another.  Author, for
   example, is a relationship associating a Document to a Person.

   Relationship names start with an uppercase letter.
-}
data NamedTypeClass = Entity Instantiability
                    | Class
                    | Relationship { rsType :: Identifier -- ^ The Entity type this relationship associates to.
                                   , rsImplies :: Identifier  -- ^ Another relationship type logically implied by this one.
                                   }

-- | Whether an Entity can be instantiated or is just a template for entities deeper in the hierarchy.
data Instantiability = Instantiable -- ^ Entity is instantiable
                     | NotInstantiable -- ^ Entity is not instantiable
                                       -- ("abstract" or "virtual", in
                                       -- OO slang)

-- | An actual type.
data Type = TypeBool -- ^ A boolean value.
          | TypeClass Identifier -- ^ A named class.
          | TypeFloat -- ^ A floating-point value.
          | TypeInt -- ^ An integer value
          | TypeList ListOrdering [Type] -- ^ A list or a set
                                         -- (depending on
                                         -- 'ListOrdering') of a given
                                         -- Type.
          | TypeLiteral Value -- ^ A literal value.  Normally used
                              -- inside a TypeUnion
          | TypeString -- ^ A string
          | TypeStruct [Map Identifier Type] -- ^ A struct, that is, a series of
                                   -- pairs of named fields
                                   -- (Identifier, Type)
          | TypeUnion [Type] -- ^ A type among a set.
          | TypeVoid -- ^ The non-type.  Declaring a field 'TypeVoid'
                     -- removes an inherited field from a type.  For
                     -- example, Document has an author field (the
                     -- author class)

-- | A value.
data Value = ValueBool Bool -- ^ A value of 'TypeBool'.
           | ValueClass Value -- ^ a value of 'TypeClass'.
           | ValueFloat Double -- ^ A value of 'TypeFloat'.
           | ValueInt Integer -- ^ A value of 'TypeInt'.
           | ValueList [Value] -- ^ A value of 'TypeList'.
           | ValueLiteral -- ^ The (empty) value of a 'TypeLiteral'.
           | ValueString String -- ^ A value of 'TypeString'.
           | ValueStruct [Map Identifier Value] -- ^ A value of 'TypeStruct'.
           | ValueUnion Int Value -- ^ A value of 'TypeUnion': the
                                  -- index of the alternative followed
                                  -- by the value itself.

data ListOrdering = Ordered | Unordered

type Identifier = String

data CompilationError = MissingParent String
                      | DuplicateName String

-- | Insert a new type inside the ontology.
insert :: Ontology -> NamedType -> Ontology
insert = undefined

-- | Compile a list of a types into a hierarchical 'Ontology'.
compile :: [NamedType] -> Either CompilationError Ontology
compile = undefined
