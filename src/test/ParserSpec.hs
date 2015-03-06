module ParserSpec (spec)
where

import Test.Hspec
import Test.QuickCheck (property)
import qualified Parser as P

spec :: Spec
spec = do
    describe "basic tests" $ do
        it "empty file" $ P.parse [] `shouldBe` Right  []