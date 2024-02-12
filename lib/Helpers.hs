module Helpers (findKey ) where
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.List (find)


findKey :: (Eq v) => v -> Map k v -> Maybe k
findKey val = fmap fst . find ((== val) . snd) . Map.toList