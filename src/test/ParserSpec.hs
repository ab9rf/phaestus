module ParserSpec (spec)
where

import Test.Hspec
import Test.QuickCheck (property)
import qualified Parser as P
import qualified Tokenizer as T

spec :: Spec
spec = do
    describe "basic tests" $ do
        it "empty file" $ P.parse [] `shouldBe` Right []
        it "HTML only" $ P.parse [T.InlineHTML "plain text"] `shouldBe` Right [P.InlineHTML (T.InlineHTML "plain text")]
        it "1;" $ P.parse [T.IntegerToken "1", T.Semicolon] 
            `shouldBe` Right [P.StmtExpression (P.ExprConstant (P.ConstantInteger (T.IntegerToken "1")))]
        it "1.0;" $ P.parse [T.RealToken "1.0", T.Semicolon] 
            `shouldBe` Right [P.StmtExpression (P.ExprConstant (P.ConstantReal (T.RealToken "1.0")))]
        it "'a';" $ P.parse [T.StringToken False "a", T.Semicolon] 
            `shouldBe` Right [P.StmtExpression (P.ExprConstant (P.ConstantString (T.StringToken False "a")))]
        it "\"a\";" $ P.parse [T.StartInterpolatedString False, T.StringFragment "a", T.EndInterpolatedString, T.Semicolon] 
            `shouldBe` Right [P.StmtExpression (P.ExprConstant (P.ConstantString (T.StringFragment "a")))]
        it "nowdoc" $ P.parse [T.NowDoc False "A" "1", T.Semicolon] 
            `shouldBe` Right [P.StmtExpression (P.ExprConstant (P.ConstantString (T.NowDoc False "A" "1")))]
        it "CONSTANT;" $ P.parse [T.IdentToken "CONSTANT", T.Semicolon] 
            `shouldBe` Right [P.StmtExpression (P.ExprConstant (P.ConstantFromIdentifier (T.IdentToken "CONSTANT")))]
        it "(1);" $ P.parse [T.LParen, T.IntegerToken "1", T.RParen, T.Semicolon] 
            `shouldBe` Right [P.StmtExpression (P.ExprConstant (P.ConstantInteger (T.IntegerToken "1")))]
        