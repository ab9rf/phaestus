module ParserSpec (spec)
where

import Test.Hspec
import Test.QuickCheck (property)
import qualified Parser as P
import qualified Tokenizer as T
import qualified AST as A

spec :: Spec
spec = do
    describe "basic tests" $ do
        it "empty file" $ P.parse [] `shouldBe` Right []
        it "HTML only" $ P.parse [T.InlineHTML "plain text"] `shouldBe` Right [A.InlineHTML (T.InlineHTML "plain text")]
    describe "expressions" $ do
        it "1;" $ P.parse [T.IntegerToken "1", T.Semicolon] 
            `shouldBe` Right [A.StmtExpression (A.ExprConstant (A.ConstantInteger (T.IntegerToken "1")))]
        it "1.0;" $ P.parse [T.RealToken "1.0", T.Semicolon] 
            `shouldBe` Right [A.StmtExpression (A.ExprConstant (A.ConstantReal (T.RealToken "1.0")))]
        it "'a';" $ P.parse [T.StringToken False "a", T.Semicolon] 
            `shouldBe` Right [A.StmtExpression (A.ExprConstant (A.ConstantString (T.StringToken False "a")))]
        it "\"a\";" $ P.parse [T.StartInterpolatedString False, T.StringFragment "a", T.EndInterpolatedString, T.Semicolon] 
            `shouldBe` Right [A.StmtExpression (A.ExprConstant (A.ConstantString (T.StringFragment "a")))]
        it "nowdoc" $ P.parse [T.NowDoc False "A" "1", T.Semicolon] 
            `shouldBe` Right [A.StmtExpression (A.ExprConstant (A.ConstantString (T.NowDoc False "A" "1")))]
        it "CONSTANT;" $ P.parse [T.IdentToken "CONSTANT", T.Semicolon] 
            `shouldBe` Right [A.StmtExpression (A.ExprConstant (A.ConstantFromIdentifier (T.IdentToken "CONSTANT")))]
        it "(1);" $ P.parse [T.LParen, T.IntegerToken "1", T.RParen, T.Semicolon] 
            `shouldBe` Right [A.StmtExpression (A.ExprConstant (A.ConstantInteger (T.IntegerToken "1")))]
        it "clone 1;" $ P.parse [T.KeywordClone, T.IntegerToken "1", T.Semicolon] 
            `shouldBe` Right [A.StmtExpression (A.ExprUnaryOp A.Clone (A.ExprConstant (A.ConstantInteger (T.IntegerToken "1"))))]
        it "$a;" $ P.parse [T.VariableToken "a", T.Semicolon] 
            `shouldBe` Right [A.StmtExpression (A.ExprVariable (A.VariableSimple (T.VariableToken "a")))]
        it "$a[1];" $ P.parse [T.VariableToken "a", T.LBracket, T.IntegerToken "1", T.RBracket, T.Semicolon] 
            `shouldBe` Right [A.StmtExpression (A.ExprVariable (A.VariableOffset (A.VariableSimple (T.VariableToken "a")) (A.ExprConstant (A.ConstantInteger (T.IntegerToken "1")))))]
        it "$a**2;" $ P.parse [T.VariableToken "a", T.OpPow, T.IntegerToken "2", T.Semicolon]
            `shouldBe` Right [A.StmtExpression (A.ExprBinaryOp A.Power (A.ExprVariable (A.VariableSimple (T.VariableToken "a"))) (A.ExprConstant (A.ConstantInteger (T.IntegerToken "2"))))]
        it "$a++" $ P.parse [T.VariableToken "a", T.OpInc, T.Semicolon]
            `shouldBe` Right [A.StmtExpression (A.ExprPPID A.PostIncrement (A.VariableSimple (T.VariableToken "a")))]
        it "$a--" $ P.parse [T.VariableToken "a", T.OpDec, T.Semicolon]
            `shouldBe` Right [A.StmtExpression (A.ExprPPID A.PostDecrement (A.VariableSimple (T.VariableToken "a")))]        
        it "++$a" $ P.parse [T.OpInc, T.VariableToken "a", T.Semicolon]
            `shouldBe` Right [A.StmtExpression (A.ExprPPID A.PreIncrement (A.VariableSimple (T.VariableToken "a")))]
        it "--$a" $ P.parse [T.OpDec, T.VariableToken "a", T.Semicolon]
            `shouldBe` Right [A.StmtExpression (A.ExprPPID A.PreDecrement (A.VariableSimple (T.VariableToken "a")))]        
        it "~$a" $ P.parse [T.OpTilde, T.VariableToken "a", T.Semicolon]
            `shouldBe` Right [A.StmtExpression (A.ExprUnaryOp A.BinaryNegate (A.ExprVariable (A.VariableSimple (T.VariableToken "a"))))]        
        it "@$a" $ P.parse [T.OpAtSign, T.VariableToken "a", T.Semicolon]
            `shouldBe` Right [A.StmtExpression (A.ExprUnaryOp A.SuppressError (A.ExprVariable (A.VariableSimple (T.VariableToken "a"))))]        
        it "(int) $a" $ P.parse [T.CastInt, T.VariableToken "a", T.Semicolon]
            `shouldBe` Right [A.StmtExpression (A.ExprUnaryOp A.CastInt (A.ExprVariable (A.VariableSimple (T.VariableToken "a"))))]        
        it "(double) $a" $ P.parse [T.CastReal, T.VariableToken "a", T.Semicolon]
            `shouldBe` Right [A.StmtExpression (A.ExprUnaryOp A.CastReal (A.ExprVariable (A.VariableSimple (T.VariableToken "a"))))]        
        it "(string) $a" $ P.parse [T.CastString, T.VariableToken "a", T.Semicolon]
            `shouldBe` Right [A.StmtExpression (A.ExprUnaryOp A.CastString (A.ExprVariable (A.VariableSimple (T.VariableToken "a"))))]        
        it "(array) $a" $ P.parse [T.CastArray, T.VariableToken "a", T.Semicolon]
            `shouldBe` Right [A.StmtExpression (A.ExprUnaryOp A.CastArray (A.ExprVariable (A.VariableSimple (T.VariableToken "a"))))]        
        it "(object) $a" $ P.parse [T.CastObject, T.VariableToken "a", T.Semicolon]
            `shouldBe` Right [A.StmtExpression (A.ExprUnaryOp A.CastObject (A.ExprVariable (A.VariableSimple (T.VariableToken "a"))))]        
        it "(bool) $a" $ P.parse [T.CastBool, T.VariableToken "a", T.Semicolon]
            `shouldBe` Right [A.StmtExpression (A.ExprUnaryOp A.CastBool (A.ExprVariable (A.VariableSimple (T.VariableToken "a"))))]        
        it "(unset) $a" $ P.parse [T.CastUnset, T.VariableToken "a", T.Semicolon]
            `shouldBe` Right [A.StmtExpression (A.ExprUnaryOp A.CastUnset (A.ExprVariable (A.VariableSimple (T.VariableToken "a"))))]        
