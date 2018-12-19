module Specs
  ( specFromExamples
  , specItem
  ) where

import Test.Hspec (SpecWith)
import Test.Hspec.Core.Spec (SpecTree, fromSpecList, specItem)

specFromExamples :: [a] -> (a -> SpecTree b) -> SpecWith b
specFromExamples examples builder = fromSpecList $ map builder examples
