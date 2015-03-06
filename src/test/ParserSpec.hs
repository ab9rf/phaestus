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