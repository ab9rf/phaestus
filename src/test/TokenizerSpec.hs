module TokenizerSpec (spec)
where

import Test.Hspec
import Test.QuickCheck (property)
import qualified Tokenizer as T

spec :: Spec
spec = do
    describe "basic tests" $ do
        it "empty file" $ T.tokenize "" `shouldBe` []
        it "plain html" $ T.tokenize "plain text" `shouldBe` [T.InlineHTML "plain text"]
        it "empty code" $ T.tokenize "<? ?>" `shouldBe` [T.Semicolon]
        it "echo start" $ T.tokenize "<?= " `shouldBe` [T.KeywordEcho]
        it "unclosed script block" $ T.tokenize "<?" `shouldBe` []
        it "unclosed script block with newlines" $ T.tokenize "<?\n" `shouldBe` []
        it "invalid" $ T.tokenize "<? \031" `shouldBe` [T.Invalid "\031"]
        it "obscure script start" $ T.tokenize "<script language='php'> </script>"
            `shouldBe` [T.Semicolon]
    describe "casts" $ do
        it "int casts" $ T.tokenize "<? (int) (integer)" `shouldBe` [T.CastInt, T.CastInt]
        it "float casts" $ T.tokenize "<? (real) (float) (double)"
                `shouldBe` [T.CastReal, T.CastReal, T.CastReal]
        it "boolean casts" $ T.tokenize "<? (bool) (boolean)"
                `shouldBe` [T.CastBool, T.CastBool]
        it "other casts" $ T.tokenize "<? (string) (array) (object) (unset)"
                `shouldBe` [T.CastString, T.CastArray, T.CastObject, T.CastUnset]
    describe "multiple character operators" $ do
        it "==" $ T.tokenize "<? ==" `shouldBe` [T.OpEqEq]
        it "===" $ T.tokenize "<? ===" `shouldBe` [T.OpEqEqEq]
        it "!=" $ T.tokenize "<? !=" `shouldBe` [T.OpNotEq]
        it "<>" $ T.tokenize "<? <>" `shouldBe` [T.OpNotEq]
        it "!==" $T.tokenize "<? !==" `shouldBe` [T.OpNotEqEq]
        it "<=" $ T.tokenize "<? <=" `shouldBe` [T.OpLE]
        it ">=" $ T.tokenize "<? >=" `shouldBe` [T.OpGE]
        it "++" $ T.tokenize "<? ++" `shouldBe` [T.OpInc]
        it "--" $ T.tokenize "<? --" `shouldBe` [T.OpDec]
        it "=>" $ T.tokenize "<? =>" `shouldBe` [T.OpDoubleArrow]
        it "->" $ T.tokenize "<? ->" `shouldBe` [T.OpSingleArrow]
        it "<<" $ T.tokenize "<? <<" `shouldBe` [T.OpSL]
        it ">>" $ T.tokenize "<? >>" `shouldBe` [T.OpSR]
        it "+=" $ T.tokenize "<? +=" `shouldBe` [T.OpPlusEq]
        it "-=" $ T.tokenize "<? -=" `shouldBe` [T.OpMinusEq]
        it "*=" $ T.tokenize "<? *=" `shouldBe` [T.OpMultEq]
        it "/=" $ T.tokenize "<? /=" `shouldBe` [T.OpDivEq]
        it ".=" $ T.tokenize "<? .=" `shouldBe` [T.OpConcatEq]
        it "%=" $ T.tokenize "<? %=" `shouldBe` [T.OpModEq]
        it "&=" $ T.tokenize "<? &=" `shouldBe` [T.OpAndEq]
        it "|=" $ T.tokenize "<? |=" `shouldBe` [T.OpOrEq]
        it "^=" $ T.tokenize "<? ^=" `shouldBe` [T.OpXorEq]
        it "<<=" $ T.tokenize "<? <<=" `shouldBe` [T.OpSLEq]
        it ">>=" $ T.tokenize "<? >>=" `shouldBe` [T.OpSREq]
        it "::" $ T.tokenize "<? ::" `shouldBe` [T.OpColonColon]
        it "&&" $ T.tokenize "<? &&" `shouldBe` [T.OpLogicAnd]
        it "||" $ T.tokenize "<? ||" `shouldBe` [T.OpLogicOr]
    describe "punctuation" $ do
        it "(" $ T.tokenize "<? (" `shouldBe` [T.LParen]
        it ")" $ T.tokenize "<? )" `shouldBe` [T.RParen]
        it "{" $ T.tokenize "<? {" `shouldBe` [T.LBrace]
        it "}" $ T.tokenize "<? }" `shouldBe` [T.RBrace]
        it "[" $ T.tokenize "<? [" `shouldBe` [T.LBracket]
        it "]" $ T.tokenize "<? ]" `shouldBe` [T.RBracket]
        it "+" $ T.tokenize "<? +" `shouldBe` [T.OpPlus]
        it "-" $ T.tokenize "<? -" `shouldBe` [T.OpMinus]
        it "/" $ T.tokenize "<? /" `shouldBe` [T.OpSlash]
        it "*" $ T.tokenize "<? *" `shouldBe` [T.OpStar]
        it "%" $ T.tokenize "<? %" `shouldBe` [T.OpPercent]
        it "^" $ T.tokenize "<? ^" `shouldBe` [T.OpCaret]
        it "&" $ T.tokenize "<? &" `shouldBe` [T.OpAmpersand]
        it "|" $ T.tokenize "<? |" `shouldBe` [T.OpPipe]
        it "~" $ T.tokenize "<? ~" `shouldBe` [T.OpTilde]
        it "=" $ T.tokenize "<? =" `shouldBe` [T.OpEq]
        it "<" $ T.tokenize "<? <" `shouldBe` [T.OpLt]
        it ">" $ T.tokenize "<? >" `shouldBe` [T.OpGt]
        it "." $ T.tokenize "<? ." `shouldBe` [T.OpDot]
        it "!" $ T.tokenize "<? !" `shouldBe` [T.OpBang]
        it "," $ T.tokenize "<? ," `shouldBe` [T.OpComma]
        it "?" $ T.tokenize "<? ?" `shouldBe` [T.OpQuestion]
        it ":" $ T.tokenize "<? :" `shouldBe` [T.OpColon]
        it "@" $ T.tokenize "<? @" `shouldBe` [T.OpAtSign]
        it "$" $ T.tokenize "<? $" `shouldBe` [T.OpDollars]
        it ";" $ T.tokenize "<? ;" `shouldBe` [T.Semicolon]
        it "\\" $ T.tokenize "<? \\" `shouldBe` [T.Backslash]
    describe "keywords & identifiers" $ do
        it "$identifer" $ T.tokenize "<? $identifier" `shouldBe` [T.VariableToken "identifier"]
        it "keyword" $ T.tokenize "<? keyword" `shouldBe` [T.IdentToken "keyword"]
        it "keyword with ascii-8 char" $ T.tokenize "<? \129\130\131"
            `shouldBe` [T.IdentToken "\129\130\131"]
        it "keyword with digits" $ T.tokenize "<? a123" `shouldBe` [T.IdentToken "a123"]
        it "keyword and" $ T.tokenize "<? and" `shouldBe` [T.KeywordAnd]
        it "keyword or" $ T.tokenize "<? or" `shouldBe` [T.KeywordOr]
        it "keyword xor" $ T.tokenize "<? xor" `shouldBe` [T.KeywordXor]
        it "keyword __FILE__" $ T.tokenize "<? __FILE__" `shouldBe` [T.Keyword'FILE]
        it "keyword __LINE__" $ T.tokenize "<? __LINE__" `shouldBe` [T.Keyword'LINE]
        it "keyword __DIR__" $ T.tokenize "<? __DIR__" `shouldBe` [T.Keyword'DIR]
        it "keyword array" $ T.tokenize "<? array" `shouldBe` [T.KeywordArray]
        it "keyword as" $ T.tokenize "<? as" `shouldBe` [T.KeywordAs]
        it "keyword break" $ T.tokenize "<? break" `shouldBe` [T.KeywordBreak]
        it "keyword case" $ T.tokenize "<? case" `shouldBe` [T.KeywordCase]
        it "keyword class" $ T.tokenize "<? class" `shouldBe` [T.KeywordClass]
        it "keyword const" $ T.tokenize "<? const" `shouldBe` [T.KeywordConst]
        it "keyword continue" $ T.tokenize "<? continue" `shouldBe` [T.KeywordContinue]
        it "keyword declare" $ T.tokenize "<? declare" `shouldBe` [T.KeywordDeclare]
        it "keyword default" $ T.tokenize "<? default" `shouldBe` [T.KeywordDefault]
        it "keyword do" $ T.tokenize "<? do" `shouldBe` [T.KeywordDo]
        it "keyword echo" $ T.tokenize "<? echo" `shouldBe` [T.KeywordEcho]
        it "keyword else" $ T.tokenize "<? else" `shouldBe` [T.KeywordElse]
        it "keyword elseif" $ T.tokenize "<? elseif" `shouldBe` [T.KeywordElseif]
        it "keyword empty" $ T.tokenize "<? empty" `shouldBe` [T.KeywordEmpty]
        it "keyword enddeclare" $ T.tokenize "<? enddeclare" `shouldBe` [T.KeywordEnddeclare]
        it "keyword endfor" $ T.tokenize "<? endfor" `shouldBe` [T.KeywordEndfor]
        it "keyword endforeach" $ T.tokenize "<? endforeach" `shouldBe` [T.KeywordEndforeach]
        it "keyword endif" $ T.tokenize "<? endif" `shouldBe` [T.KeywordEndif]
        it "keyword endswitch" $ T.tokenize "<? endswitch" `shouldBe` [T.KeywordEndswitch]
        it "keyword endwhile" $ T.tokenize "<? endwhile" `shouldBe` [T.KeywordEndwhile]
        it "keyword eval" $ T.tokenize "<? eval" `shouldBe` [T.KeywordEval]
        it "keyword exit" $ T.tokenize "<? exit" `shouldBe` [T.KeywordExit]
        it "keyword die" $ T.tokenize "<? die" `shouldBe` [T.KeywordDie]
        it "keyword extends" $ T.tokenize "<? extends" `shouldBe` [T.KeywordExtends]
        it "keyword for" $ T.tokenize "<? for" `shouldBe` [T.KeywordFor]
        it "keyword foreach" $ T.tokenize "<? foreach" `shouldBe` [T.KeywordForeach]
        it "keyword function" $ T.tokenize "<? function" `shouldBe` [T.KeywordFunction]
        it "keyword global" $ T.tokenize "<? global" `shouldBe` [T.KeywordGlobal]
        it "keyword if" $ T.tokenize "<? if" `shouldBe` [T.KeywordIf]
        it "keyword include" $ T.tokenize "<? include" `shouldBe` [T.KeywordInclude]
        it "keyword include_once" $ T.tokenize "<? include_once" `shouldBe` [T.KeywordIncludeOnce]
        it "keyword instanceof" $ T.tokenize "<? instanceof" `shouldBe` [T.KeywordInstanceOf]
        it "keyword isset" $ T.tokenize "<? isset" `shouldBe` [T.KeywordIsset]
        it "keyword list" $ T.tokenize "<? list" `shouldBe` [T.KeywordList]
        it "keyword new" $ T.tokenize "<? new" `shouldBe` [T.KeywordNew]
        it "keyword print" $ T.tokenize "<? print" `shouldBe` [T.KeywordPrint]
        it "keyword require" $ T.tokenize "<? require" `shouldBe` [T.KeywordRequire]
        it "keyword require_once" $ T.tokenize "<? require_once" `shouldBe` [T.KeywordRequireOnce]
        it "keyword return" $ T.tokenize "<? return" `shouldBe` [T.KeywordReturn]
        it "keyword static" $ T.tokenize "<? static" `shouldBe` [T.KeywordStatic]
        it "keyword switch" $ T.tokenize "<? switch" `shouldBe` [T.KeywordSwitch]
        it "keyword unset" $ T.tokenize "<? unset" `shouldBe` [T.KeywordUnset]
        it "keyword use" $ T.tokenize "<? use" `shouldBe` [T.KeywordUse]
        it "keyword var" $ T.tokenize "<? var" `shouldBe` [T.KeywordVar]
        it "keyword while" $ T.tokenize "<? while" `shouldBe` [T.KeywordWhile]
        it "keyword __FUNCTION__" $ T.tokenize "<? __FUNCTION__" `shouldBe` [T.Keyword'FUNCTION]
        it "keyword __CLASS__" $ T.tokenize "<? __CLASS__" `shouldBe` [T.Keyword'CLASS]
        it "keyword __METHOD__" $ T.tokenize "<? __METHOD__" `shouldBe` [T.Keyword'METHOD]
        it "keyword final" $ T.tokenize "<? final" `shouldBe` [T.KeywordFinal]
        it "keyword interface" $ T.tokenize "<? interface" `shouldBe` [T.KeywordInterface]
        it "keyword implements" $ T.tokenize "<? implements" `shouldBe` [T.KeywordImplements]
        it "keyword public" $ T.tokenize "<? public" `shouldBe` [T.KeywordPublic]
        it "keyword private" $ T.tokenize "<? private" `shouldBe` [T.KeywordPrivate]
        it "keyword protected" $ T.tokenize "<? protected" `shouldBe` [T.KeywordProtected]
        it "keyword abstract" $ T.tokenize "<? abstract" `shouldBe` [T.KeywordAbstract]
        it "keyword clone" $ T.tokenize "<? clone" `shouldBe` [T.KeywordClone]
        it "keyword try" $ T.tokenize "<? try" `shouldBe` [T.KeywordTry]
        it "keyword catch" $ T.tokenize "<? catch" `shouldBe` [T.KeywordCatch]
        it "keyword throw" $ T.tokenize "<? throw" `shouldBe` [T.KeywordThrow]
        it "keyword namespace" $ T.tokenize "<? namespace" `shouldBe` [T.KeywordNamespace]
        it "keyword goto" $ T.tokenize "<? goto" `shouldBe` [T.KeywordGoto]
        it "keyword finally" $ T.tokenize "<? finally" `shouldBe` [T.KeywordFinally]
        it "keyword trait" $ T.tokenize "<? trait" `shouldBe` [T.KeywordTrait]
        it "keyword callable" $ T.tokenize "<? callable" `shouldBe` [T.KeywordCallable]
        it "keyword insteadof" $ T.tokenize "<? insteadof" `shouldBe` [T.KeywordInsteadof]
        it "keyword yield" $ T.tokenize "<? yield" `shouldBe` [T.KeywordYield]
        it "keyword __TRAIT__" $ T.tokenize "<? __TRAIT__" `shouldBe` [T.Keyword'TRAIT]
        it "keyword __NAMESPACE__" $ T.tokenize "<? __NAMESPACE__" `shouldBe` [T.Keyword'NAMESPACE]
    describe "floating constants" $ do
        it "real" $ T.tokenize "<? 123.456" `shouldBe` [T.RealToken "123.456"]
        it "real" $ T.tokenize "<? 123." `shouldBe` [T.RealToken "123."]
        it "real" $ T.tokenize "<? .456" `shouldBe` [T.RealToken ".456"]
        it "real" $ T.tokenize "<? 123E4" `shouldBe` [T.RealToken "123E4"]
        it "real" $ T.tokenize "<? 123.456E9" `shouldBe` [T.RealToken "123.456E9"]
        it "real" $ T.tokenize "<? 123.E9" `shouldBe` [T.RealToken "123.E9"]
        it "real" $ T.tokenize "<? .456E9" `shouldBe` [T.RealToken ".456E9"]
        it "real" $ T.tokenize "<? 123E-4" `shouldBe` [T.RealToken "123E-4"]
        it "real" $ T.tokenize "<? 123.456E-9" `shouldBe` [T.RealToken "123.456E-9"]
        it "real" $ T.tokenize "<? 123.E-9" `shouldBe` [T.RealToken "123.E-9"]
        it "real" $ T.tokenize "<? .456E-9" `shouldBe` [T.RealToken ".456E-9"]
        it "real" $ T.tokenize "<? 123E+4" `shouldBe` [T.RealToken "123E+4"]
        it "real" $ T.tokenize "<? 123.456E+9" `shouldBe` [T.RealToken "123.456E+9"]
        it "real" $ T.tokenize "<? 123.E+9" `shouldBe` [T.RealToken "123.E+9"]
        it "real" $ T.tokenize "<? .456E+9" `shouldBe` [T.RealToken ".456E+9"]
    describe "integer constants" $ do
        it "integer (decimal)" $ T.tokenize "<? 123" `shouldBe` [T.IntegerToken "123"]
        it "integer (binary)" $ T.tokenize "<? 0b01000101" `shouldBe` [T.IntegerToken "0b01000101"]
        it "integer (hex)" $ T.tokenize "<? 0xdeadbabe" `shouldBe` [T.IntegerToken "0xdeadbabe"]
        it "integer (hex w/ caps)" $ T.tokenize "<? 0xdeadBABE" `shouldBe` [T.IntegerToken "0xdeadBABE"]
        it "integer (octal)" $ T.tokenize "<? 07004" `shouldBe` [T.IntegerToken "07004"]
    describe "comments" $ do
        it "comment (block)" $ T.tokenize "<? /* test */" `shouldBe` []
        it "comment (block)" $ T.tokenize "<? /* test \n test\n test */" `shouldBe` []
        it "comment (block)" $ T.tokenize "<? /* testa /* testb */" `shouldBe` []
        it "comment (line)" $ T.tokenize "<? // test \n" `shouldBe` []
        it "comment (line)" $ T.tokenize "<? # test \n" `shouldBe` []
        it "comment (line)" $ T.tokenize "<? // test" `shouldBe` []
        it "comment (line)" $ T.tokenize "<? # test" `shouldBe` []
    describe "single-quoted strings" $ do
        it "sq str empty" $ T.tokenize "<? ''" `shouldBe` [T.StringToken False ""]
        it "sq str nonempty" $ T.tokenize "<? 'test'" `shouldBe` [T.StringToken False "test"]
        it "sq str quote" $ T.tokenize "<? '\\''" `shouldBe` [T.StringToken False "'"]
        it "sq str backslash" $ T.tokenize "<? '\\\\'" `shouldBe` [T.StringToken False "\\"]
        it "sq str newline" $ T.tokenize "<? '\n'" `shouldBe` [T.StringToken False "\n"]
        it "sq str binary" $ T.tokenize "<? b'test'" `shouldBe` [T.StringToken True "test"]
        it "sq str pointless backslash" $ T.tokenize "<? '\\x'" `shouldBe` [T.StringToken False "\\x"]
    describe "double-quoted strings" $ do
        it "dq str empty" $ T.tokenize "<? \"\"" `shouldBe` [T.StartInterpolatedString False, T.EndInterpolatedString]
        it "dq str nonempty" $ T.tokenize "<? \"test\""
            `shouldBe` [T.StartInterpolatedString False, T.StringFragment "test", T.EndInterpolatedString]
        it "dq str nonempty" $ T.tokenize "<? \"test\";"
            `shouldBe` [T.StartInterpolatedString False, T.StringFragment "test", T.EndInterpolatedString, T.Semicolon]
        it "dq str quote" $ T.tokenize "<? \"\\\"\""
            `shouldBe` [T.StartInterpolatedString False, T.StringFragment "\"", T.EndInterpolatedString]
        it "dq str backslash" $ T.tokenize "<? \"\\\\\""
            `shouldBe` [T.StartInterpolatedString False, T.StringFragment "\\", T.EndInterpolatedString]
        it "dq str newline" $ T.tokenize "<? \"\n\""
            `shouldBe` [T.StartInterpolatedString False, T.StringFragment "\n", T.EndInterpolatedString]
        it "dq str binary" $ T.tokenize "<? b\"test\""
            `shouldBe` [T.StartInterpolatedString True, T.StringFragment "test", T.EndInterpolatedString]
        it "dq str with simple var sub" $ T.tokenize "<? \"the $cat is $red\""
            `shouldBe` [T.StartInterpolatedString False, T.StringFragment "the ", T.InterpolatedVariable "cat", T.StringFragment " is ", T.InterpolatedVariable "red", T.EndInterpolatedString]
        it "dq str with ${} var sub" $ T.tokenize "<? \"the ${cat}s are ${red}\""
            `shouldBe` [T.StartInterpolatedString False, T.StringFragment "the ", T.InterpolatedVariable "cat", T.StringFragment "s are ", T.InterpolatedVariable "red", T.EndInterpolatedString]
        it "dq str with {$...} sub" $ T.tokenize "<? \"the {$cat}s are {$red}\""
            `shouldBe` [T.StartInterpolatedString False, T.StringFragment "the ", T.LBrace, T.VariableToken "cat", T.RBrace, T.StringFragment "s are ", T.LBrace, T.VariableToken "red", T.RBrace, T.EndInterpolatedString]
        it "dq string escape sequences" $ T.tokenize "<? \"\\n\\r\\t\\v\\e\\f\\$\""
            `shouldBe` [T.StartInterpolatedString False, T.StringFragment "\n\r\t\v\027\f$", T.EndInterpolatedString]
        it "dq string octal" $ T.tokenize "<? \"\\0\\4\\10\\100\\108\""
            `shouldBe` [T.StartInterpolatedString False, T.StringFragment "\0\4\8\64\8\56", T.EndInterpolatedString]
        it "dq string hex" $ T.tokenize "<? \"\\x0\\x4\\x8\\x40\\x088\\xfff\\xFFF\""
            `shouldBe` [T.StartInterpolatedString False, T.StringFragment "\0\4\8\64\8\56\255f\255F", T.EndInterpolatedString]
        it "dq str with var sub with property" $ T.tokenize "<? \"$cat->tail\""
            `shouldBe` [T.StartInterpolatedString False, T.InterpolatedVariable "cat", T.InterpolatedProperty "tail", T.EndInterpolatedString]
        it "dq str with var sub with array, keyword index" $ T.tokenize "<? \"$cat[tail]\""
            `shouldBe` [T.StartInterpolatedString False, T.InterpolatedVariable "cat", T.InterpolatedIndexIdent "tail", T.EndInterpolatedString]
        it "dq str with var sub with array, integer index" $ T.tokenize "<? \"$cat[30]\""
            `shouldBe` [T.StartInterpolatedString False, T.InterpolatedVariable "cat", T.InterpolatedIndexInt "30", T.EndInterpolatedString]

    describe "backquote" $ do
        it "bq str empty" $ T.tokenize "<? ``" `shouldBe` [T.Backquote, T.Backquote]
        it "bq str nonempty" $ T.tokenize "<? `test`"
            `shouldBe` [T.Backquote, T.StringFragment "test", T.Backquote]
        it "bq str nonempty" $ T.tokenize "<? `test`;"
            `shouldBe` [T.Backquote, T.StringFragment "test", T.Backquote, T.Semicolon]
        it "bq str quote" $ T.tokenize "<? `\\``"
            `shouldBe` [T.Backquote, T.StringFragment "`", T.Backquote]
        it "bq str backslash" $ T.tokenize "<? `\\\\`"
            `shouldBe` [T.Backquote, T.StringFragment "\\", T.Backquote]
        it "bq str newline" $ T.tokenize "<? `\n`"
            `shouldBe` [T.Backquote, T.StringFragment "\n", T.Backquote]
        it "bq str with simple var sub" $ T.tokenize "<? `the $cat is $red`"
            `shouldBe` [T.Backquote, T.StringFragment "the ", T.InterpolatedVariable "cat", T.StringFragment " is ", T.InterpolatedVariable "red", T.Backquote]
    describe "brace tests" $ do
        it "open/close brace *1" $ T.tokenize "<? { 1 }" `shouldBe` [T.LBrace, T.IntegerToken "1", T.RBrace]
        it "open/close brace *2" $ T.tokenize "<? { { 1 } }" `shouldBe` [T.LBrace, T.LBrace, T.IntegerToken "1", T.RBrace, T.RBrace]
        it "unbalanced braces " $ T.tokenize "<? } { { 1 } }" `shouldBe` [T.RBrace, T.LBrace, T.LBrace, T.IntegerToken "1", T.RBrace, T.RBrace]
    describe "miscellaneous tests" $ do
        it "crlf" $ T.tokenize "<? 1\r2\n3\r\n4\r\r\n5\r\n\n6 ?>"
            `shouldBe` [T.IntegerToken "1", T.IntegerToken "2", T.IntegerToken "3", T.IntegerToken "4", T.IntegerToken "5", T.IntegerToken "6", T.Semicolon]
        it "newline after stop, crlf" $ T.tokenize "<? ?>\r\nA"
            `shouldBe` [T.Semicolon, T.InlineHTML "A"]
        it "newline after stop, nl only" $ T.tokenize "<? ?>\nA"
            `shouldBe` [T.Semicolon, T.InlineHTML "A"]
        it "newline after stop, cr only" $ T.tokenize "<? ?>\rA"
            `shouldBe` [T.Semicolon, T.InlineHTML "A"]
        it "newline after stop, crlf*2" $ T.tokenize "<? ?>\r\n\r\nA"
            `shouldBe` [T.Semicolon, T.InlineHTML "\r\nA"]
        it "newline after stop, lf*2" $ T.tokenize "<? ?>\n\nA"
            `shouldBe` [T.Semicolon, T.InlineHTML "\nA"]
    describe "heredocs" $ do
        it "basic nowdoc, 0 line" $ T.tokenize "<?<<<'TEST'\nTEST\n" `shouldBe` [T.NowDoc False "TEST" ""]
        it "basic nowdoc, 1 line" $ T.tokenize "<?<<<'TEST'\nline1\nTEST\n" `shouldBe` [T.NowDoc False "TEST" "line1"]
        it "basic nowdoc, 2 line" $ T.tokenize "<?<<<'TEST'\nline1\nline2\nTEST\n" `shouldBe` [T.NowDoc False "TEST" "line1\nline2"]
        it "basic heredoc, 0 line" $ T.tokenize "<?<<<TEST\nTEST\n"
            `shouldBe` [T.StartHereDoc False "TEST", T.EndHereDoc]
        it "basic heredoc, 1 line" $ T.tokenize "<?<<<TEST\nline1\nTEST\n"
            `shouldBe` [T.StartHereDoc False "TEST", T.StringFragment "line1", T.EndHereDoc]
        it "basic heredoc, 2 line" $ T.tokenize "<?<<<TEST\nline1\nline2\nTEST\n"
            `shouldBe` [T.StartHereDoc False "TEST", T.StringFragment "line1\nline2", T.EndHereDoc]
        it "basic heredoc, 1 line, quotes" $ T.tokenize "<?<<<\"TEST\"\nline1\nTEST\n"
            `shouldBe` [T.StartHereDoc False "TEST", T.StringFragment "line1", T.EndHereDoc]
        it "basic nowdoc, 1 line, binary" $ T.tokenize "<?b<<<'TEST'\nline1\nTEST\n" `shouldBe` [T.NowDoc True "TEST" "line1"]
        it "heredoc w/ backslash" $ T.tokenize "<?<<<TEST\n\\line1\nTEST\n"
            `shouldBe` [T.StartHereDoc False "TEST", T.StringFragment "\\line1", T.EndHereDoc]
        it "heredoc w/ backslash*2" $ T.tokenize "<?<<<TEST\n\\\\line1\nTEST\n"
            `shouldBe` [T.StartHereDoc False "TEST", T.StringFragment "\\line1", T.EndHereDoc]
        it "unterminated heredoc" $ T.tokenize "<?<<<TEST\nline1"
            `shouldBe` [T.StartHereDoc False "TEST", T.StringFragment "line1"]
    describe "random garbage tests" $ do
        it "random text without start code" $ property $ \x -> noerror (T.tokenize x)
        it "random text with start code" $ property $ \x -> noerror (T.tokenize ("<?" ++ x))
        

noerror :: [T.Token] -> Bool    
noerror [] = True
noerror (T.Error _:_) = False
noerror (_:xs) = noerror xs
