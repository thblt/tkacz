{-| Description: A specialization of Data.Tree.Tree with named leaves and 'String' lookup.

@TODO

-}

{-# LANGUAGE Rank2Types #-}

module Tkacz.AddressableTree where

import Data.Tree (Tree (..), Forest (..))

class Named a where
  name :: a -> String

type AddressableTree a = (Named a) => Tree a

data LookupError = NoSuchNode { lkErrParent :: String
                              , lkErrMissing :: String }

lookup :: Named a => Forest a -> String -> Either LookupError a
lookup = lookup' '.'

lookup' :: Named a => Char -> Forest a  -> String ->  Either LookupError a
lookup' c o i = go o (expandId c i)
  where
    go o (i:is) | null o'  = Left $ NoSuchNode i i
                | null is = Right $ (rootLabel . head) o'
                | otherwise = go (subForest . head $ o') is
    o' = filter (\x -> i == leafName x) o
    leafName = name . rootLabel

expandId :: Eq a => a -> [a] -> [[a]]
expandId c s = let isDot = (==) c in
  case dropWhile isDot s of
                       [] -> []
                       s' -> w : expandId c s''
                         where
                           (w, s'') = break isDot s'
