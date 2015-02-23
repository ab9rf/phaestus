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
        it "::" $
            T.tokenize "<? ::" `shouldBe` [T.OpColonColon]
        it "&&" $
            T.tokenize "<? &&" `shouldBe` [T.OpLogicAnd]
        it "||" $
            T.tokenize "<? ||" `shouldBe` [T.OpLogicOr]
        it "(" $
            T.tokenize "<? (" `shouldBe` [T.LParen]
        it ")" $
            T.tokenize "<? )" `shouldBe` [T.RParen]
        it "{" $
            T.tokenize "<? {" `shouldBe` [T.LBrace]
        it "}" $
            T.tokenize "<? }" `shouldBe` [T.RBrace]
        it "[" $
            T.tokenize "<? [" `shouldBe` [T.LBracket]
        it "]" $
            T.tokenize "<? ]" `shouldBe` [T.RBracket]
        it "+" $
            T.tokenize "<? +" `shouldBe` [T.OpPlus]
        it "-" $
            T.tokenize "<? -" `shouldBe` [T.OpMinus]
        it "/" $
            T.tokenize "<? /" `shouldBe` [T.OpSlash]
        it "*" $
            T.tokenize "<? *" `shouldBe` [T.OpStar]
        it "%" $
            T.tokenize "<? %" `shouldBe` [T.OpPercent]
        it "^" $
            T.tokenize "<? ^" `shouldBe` [T.OpCaret]
        it "&" $
            T.tokenize "<? &" `shouldBe` [T.OpAmpersand]
        it "|" $
            T.tokenize "<? |" `shouldBe` [T.OpPipe]
        it "~" $
            T.tokenize "<? ~" `shouldBe` [T.OpTilde]
        it "=" $
            T.tokenize "<? =" `shouldBe` [T.OpEq]
        it "<" $
            T.tokenize "<? <" `shouldBe` [T.OpLt]
        it ">" $
            T.tokenize "<? >" `shouldBe` [T.OpGt]
        it "." $
            T.tokenize "<? ." `shouldBe` [T.OpDot]
        it "!" $
            T.tokenize "<? !" `shouldBe` [T.OpBang]
        it "," $
            T.tokenize "<? ," `shouldBe` [T.OpComma]
        it "?" $
            T.tokenize "<? ?" `shouldBe` [T.OpQuestion]
        it ":" $
            T.tokenize "<? :" `shouldBe` [T.OpColon]
        it "@" $
            T.tokenize "<? @" `shouldBe` [T.OpAtSign]
        it "$" $
            T.tokenize "<? $" `shouldBe` [T.OpDollars]
        it ";" $
            T.tokenize "<? ;" `shouldBe` [T.Semicolon]
        it "\\" $
            T.tokenize "<? \\" `shouldBe` [T.Backslash]
        it "$identifer" $
            T.tokenize "<? $identifier" `shouldBe` [T.VariableToken "identifier"]
        it "invalid" $
            T.tokenize "<? \031" `shouldBe` [T.Invalid "\031"]
        it "obscure script start" $
            T.tokenize "<script language='php'> </script>" `shouldBe` [T.Semicolon]
        it "keyword" $
            T.tokenize "<? keyword" `shouldBe` [T.IdentToken "keyword"]
        it "keyword with ascii-8 char" $
            T.tokenize "<? \129\130\131" `shouldBe` [T.IdentToken "\129\130\131"]
        it "keyword with digits" $
            T.tokenize "<? a123" `shouldBe` [T.IdentToken "a123"]
        it "keyword and" $
            T.tokenize "<? and" `shouldBe` [T.KeywordAnd]
        