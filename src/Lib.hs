module Lib
    ()
    where

ontologyVersion = 1

type FieldName = String

data FieldType = FieldName Type

data Type =
  TypeInt Int
  | TypeString String
  | TypeFloat Double
  | TypeReference
  | TypeStruct [FieldType]
  | TypeUnion [FieldType]

type UnionIndex = Int

data Value =
  IntValue Int
  | StringValue String
  | FloatValue Double
  | UnionValue UnionIndex Value
  | StructValue [UnionIndex]

data Object = None

data RelationshipType = RelationshipType Int

data Relationship a =
  OneToOne RelationshipType Object Object
  | OneToMany RelationshipType Object [Object]
  | ManyToOne RelationshipType [Object] Object
  | ManyToMany RelationshipType [Object] Object
