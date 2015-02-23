import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import qualified Tokenizer as T

main :: IO ()
main = hspec $ do 
    describe "tokenizer" $ do
        it "empty file" $ 
            T.tokenize "" `shouldBe` []
        it "plain html" $ 
            T.tokenize "plain text" `shouldBe` [T.InlineHTML "plain text"] 
        it "empty code" $ 
            T.tokenize "<? ?>" `shouldBe` [T.Semicolon]
        it "echo start" $
            T.tokenize "<?= " `shouldBe` [T.KeywordEcho]
        it "unclosed script block" $ 
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
        it "<=" $
            T.tokenize "<? <=" `shouldBe` [T.OpLE]
        it ">=" $
            T.tokenize "<? >=" `shouldBe` [T.OpGE]
        it "++" $
            T.tokenize "<? ++" `shouldBe` [T.OpInc]
        it "--" $
            T.tokenize "<? --" `shouldBe` [T.OpDec]
        it "=>" $
            T.tokenize "<? =>" `shouldBe` [T.OpDoubleArrow]
        it "->" $
            T.tokenize "<? ->" `shouldBe` [T.OpSingleArrow]
        it "<<" $
            T.tokenize "<? <<" `shouldBe` [T.OpSL]
        it ">>" $
            T.tokenize "<? >>" `shouldBe` [T.OpSR]
        it "+=" $
            T.tokenize "<? +=" `shouldBe` [T.OpPlusEq]
        it "-=" $
            T.tokenize "<? -=" `shouldBe` [T.OpMinusEq]
        it "*=" $
            T.tokenize "<? *=" `shouldBe` [T.OpMultEq]
        it "/=" $
            T.tokenize "<? /=" `shouldBe` [T.OpDivEq]
        it ".=" $
            T.tokenize "<? .=" `shouldBe` [T.OpConcatEq]
        it "%=" $
            T.tokenize "<? %=" `shouldBe` [T.OpModEq]
        it "&=" $
            T.tokenize "<? &=" `shouldBe` [T.OpAndEq]
        it "|=" $
            T.tokenize "<? |=" `shouldBe` [T.OpOrEq]
        it "^=" $
            T.tokenize "<? ^=" `shouldBe` [T.OpXorEq]
        it "<<=" $
            T.tokenize "<? <<=" `shouldBe` [T.OpSLEq]
        it ">>=" $
            T.tokenize "<? >>=" `shouldBe` [T.OpSREq]
            