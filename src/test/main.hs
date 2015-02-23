import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import qualified Tokenizer as T

main :: IO ()
main = hspec $ do 
    describe "tokenizer" $ do
        it "empty file" $ do
            T.tokenize "" `shouldBe` []
        it "plain html" $ do
            T.tokenize "plain text" `shouldBe` [T.InlineHTML "plain text"] 
        it "empty code" $ do
            T.tokenize "<? ?>" `shouldBe` [T.Semicolon] 
        it "unclosed script block" $ do
            T.tokenize "<?" `shouldBe` []
        it "int casts" $
            T.tokenize "<? (int) (integer)" `shouldBe` [T.CastInt, T.CastInt]
        it "float casts" $
            T.tokenize "<? (real) (float) (double)" 
                `shouldBe` [T.CastReal, T.CastReal, T.CastReal]
        it "boolean casts" $
            T.tokenize "<? (bool) (boolean)"
                `shouldBe` [T.CastBool, T.CastBool]
        it "other casts" $
            T.tokenize "<? (string) (array) (object) (unset)"
                `shouldBe` [T.CastString, T.CastArray, T.CastObject, T.CastUnset]
        it "==" $
            T.tokenize "<? ==" `shouldBe` [T.OpEqEq]
        it "===" $
            T.tokenize "<? ===" `shouldBe` [T.OpEqEqEq]
        it "!=" $
            T.tokenize "<? !=" `shouldBe` [T.OpNotEq]
        it "<>" $
            T.tokenize "<? <>" `shouldBe` [T.OpNotEq]
        it "!==" $
            T.tokenize "<? !==" `shouldBe` [T.OpNotEqEq]
            
            