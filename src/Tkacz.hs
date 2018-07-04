{-|
-}

module Tkacz
  ( name,
    description,
    version,
    Explainable ) where

import qualified Paths_Tkacz

-- | The name of the beast.
name = "Tkacz"

-- | What the beast does.
description = "The strange reference manager"

-- | The beast's version number.
version = Paths_Tkacz.version

class Explainable a where
  explain :: a -> String
