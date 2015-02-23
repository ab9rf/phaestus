import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import qualified Tokenizer as T

main :: IO ()
main = hspec $ do 
    describe "tokenizer" $ do
        it "empty file" $ do
            T.tokenize "" `shouldBe` [T.EOF]
        it "plain html" $ do
            T.tokenize "plain text" `shouldBe` [T.InlineHTML "plain text", T.EOF] 
        it "empty code" $ do
            T.tokenize "<? ?>" `shouldBe` [T.Semicolon, T.EOF] 
        it "unclosed script block" $ do
            T.tokenize "<?" `shouldBe` [T.EOF]
        it "int casts" $
            T.tokenize "<? (int) (integer)" `shouldBe` [T.CastInt, T.CastInt, T.EOF]
        it "float casts" $
            T.tokenize "<? (real) (float) (double)" 
                `shouldBe` [T.CastReal, T.CastReal, T.CastReal, T.EOF]
        it "boolean casts" $
            T.tokenize "<? (bool) (boolean)"
                `shouldBe` [T.CastBool, T.CastBool, T.EOF]
        it "other casts" $
            T.tokenize "<? (string) (array) (object) (unset)"
                `shouldBe` [T.CastString, T.CastArray, T.CastObject, T.CastUnset, T.EOF]
        it "==" $
            T.tokenize "<? ==" `shouldBe` [T.OpEqEq, T.EOF]
        it "===" $
            T.tokenize "<? ===" `shouldBe` [T.OpEqEqEq, T.EOF]
        it "!=" $
            T.tokenize "<? !=" `shouldBe` [T.OpNotEq, T.EOF]
        it "<>" $
            T.tokenize "<? <>" `shouldBe` [T.OpNotEq, T.EOF]
        it "!==" $
            T.tokenize "<? !==" `shouldBe` [T.OpNotEqEq, T.EOF]
            
            