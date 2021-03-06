module FRP.Chimera.Environment.Spatial 
  (
    EnvironmentWrapping (..)
  ) where

data EnvironmentWrapping = ClipToMax 
                         | WrapHorizontal 
                         | WrapVertical 
                         | WrapBoth deriving (Show, Read)