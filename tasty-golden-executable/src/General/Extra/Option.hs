module General.Extra.Option where

import Test.Tasty (TestTree, adjustOption, localOption)
import Test.Tasty.Options (IsOption)

-- | Existential type of test options.
data SomeOption
  = forall v. (IsOption v) => LocalOption v
  | forall v. (IsOption v) => AdjustOption (v -> v)
  | forall v. (IsOption v, Semigroup v) => AppendOption v

-- | Apply a test option.
someLocalOption :: SomeOption -> TestTree -> TestTree
someLocalOption (LocalOption value) = localOption value
someLocalOption (AdjustOption adjust) = adjustOption adjust
someLocalOption (AppendOption value) = adjustOption (<> value)

-- | Apply a list of test options.
someLocalOptions :: [SomeOption] -> TestTree -> TestTree
someLocalOptions someOptions testTree = foldr someLocalOption testTree someOptions
