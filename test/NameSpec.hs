module NameSpec (spec) where

import Test.Hspec
import Common.Name
import Common.ColorScheme
import Syntax.Lexer
import Syntax.Lexeme
import qualified Common.Range as R
import qualified Data.ByteString.Char8 as BC
import Control.Monad (forM_)

spec :: Spec
spec = do
  let testLex s = do
        let input = BC.pack s
            res = lexing (R.Source "test" input) 1 input
        case res of
          (Lexeme _ (LexIdOp name) : _) -> return (Just name)
          (Lexeme _ (LexId name) : _)   -> return (Just name)
          (Lexeme _ (LexCons name _) : _) -> return (Just name)
          _ -> return Nothing

  describe "Common.Name" $ do
    it "pretty prints operators correctly" $ do
      let n1 = newName "|->"
      show (prettyName defaultColorScheme n1) `shouldBe` "(|->)"

    it "postpends unique identifiers to operators correctly" $ do
      let n1 = newName "|->"
      let n2 = toUniqueName 1 n1
      show (prettyName defaultColorScheme n2) `shouldBe` "(@x1|->)"

    it "prepends identifiers to modified operators correctly" $ do
      let n1 = newName "|->"
      let n2 = toUniqueName 1 n1
      let n3 = makeHiddenName "lift" n2
      show (prettyName defaultColorScheme n3) `shouldBe` "(@lift-@x1|->)"

    it "postpends unique identifiers to regular names correctly" $ do
      let n1 = newName "foo"
      let n2 = toUniqueName 2 n1
      show (prettyName defaultColorScheme n2) `shouldBe` "foo@2"

    it "handles division operator correctly" $ do
      let n1 = newName "/"
      show (prettyName defaultColorScheme n1) `shouldBe` "(/)"
      let n2 = toUniqueName 1 n1
      show (prettyName defaultColorScheme n2) `shouldBe` "(@x1/)"

    it "lexes modified operators correctly" $ do
      let n1 = newName "|->"
      let n2 = toUniqueName 1 n1
      let s = show (prettyName defaultColorScheme n2)
      name <- testLex s
      fmap showPlain name `shouldBe` Just (showPlain n2)

    it "lexes modified division operator correctly" $ do
      let n1 = newName "/"
      let n2 = toUniqueName 1 n1
      let s = show (prettyName defaultColorScheme n2)
      name <- testLex s
      fmap showPlain name `shouldBe` Just (showPlain n2)

    it "lexes qualified modified operators correctly" $ do
      let n1 = readQualifiedName "std/core/|->"
      let n2 = toUniqueName 1 n1
      let s = show (prettyName defaultColorScheme n2)
      s `shouldBe` "std/core/(@x1|->)"
      name <- testLex s
      fmap showPlain name `shouldBe` Just (showPlain n2)

    it "handles operators ending in hyphen correctly" $ do
      let n1 = newName "|-"
      let n2 = toUniqueName 1 n1
      show (prettyName defaultColorScheme n2) `shouldBe` "(@x1|-)"
      let s = show (prettyName defaultColorScheme n2)
      name <- testLex s
      fmap showPlain name `shouldBe` Just (showPlain n2)

    it "handles multiple postpends and prepends" $ do
      let n1 = newName "|"
      let n2 = toUniqueName 1 n1
      let n3 = toUniqueName 2 n2
      let n4 = makeHiddenName "lift" n3
      show (prettyName defaultColorScheme n4) `shouldBe` "(@lift-@x2@x1|)"
      let s = show (prettyName defaultColorScheme n4)
      name <- testLex s
      fmap showPlain name `shouldBe` Just (showPlain n4)

    it "handles complex combinations with division" $ do
      let n1 = newName "/"
      let n2 = toUniqueName 1 n1
      let n3 = makeHiddenName "special" n2
      show (prettyName defaultColorScheme n3) `shouldBe` "(@special-@x1/)"
      let s = show (prettyName defaultColorScheme n3)
      name <- testLex s
      fmap showPlain name `shouldBe` Just (showPlain n3)

    it "handles postpending a hyphenated suffix" $ do
      let n1 = newName "foo"
      let n2 = toUniqueName 1 n1
      show (prettyName defaultColorScheme n2) `shouldBe` "foo@1"
      let s = show (prettyName defaultColorScheme n2)
      name <- testLex s
      fmap showPlain name `shouldBe` Just (showPlain n2)

    describe "round-trip tests for common patterns" $ do
      let patterns =
            [ ("@lift- from toHiddenUniqueName", toHiddenUniqueName 123 "lift")
            , ("@uniq- from toHiddenUniqueName", toHiddenUniqueName 123 "uniq")
            , ("@unroll- from toHiddenUniqueName", toHiddenUniqueName 123 "unroll")
            , ("@mlift- from toHiddenUniqueName", toHiddenUniqueName 123 "mlift")
            , ("@trmc- from makeHiddenName", makeHiddenName "trmc")
            , ("@trmcm- from makeHiddenName", makeHiddenName "trmcm")
            , ("@is- from makeHiddenName", makeHiddenName "is")
            , ("@base- from makeHiddenName", makeHiddenName "base")
            , ("@new- from makeHiddenName", makeHiddenName "new")
            , ("@as- from makeHiddenName", makeHiddenName "as")
            , ("@eval- from makeHiddenName", makeHiddenName "eval")
            , ("@arg- from makeHiddenName", makeHiddenName "arg")
            , ("@default- from makeHiddenName", makeHiddenName "default")
            ]
          testRoundTrip label op nameStr =
            it (label ++ " with " ++ nameStr) $ do
              let n1 = readQualifiedName nameStr
                  n2 = op n1
                  s = show (prettyName defaultColorScheme n2)
              name <- testLex s
              fmap showPlain name `shouldBe` Just (showPlain n2)

      forM_ patterns $ \(label, op) -> do
        testRoundTrip label op "foo"
        testRoundTrip label op "(|->)"
        testRoundTrip label op "(/)"
        testRoundTrip label op "std/core/(|->)"

    describe "round-trip tests for constructor name patterns" $ do
      -- These are names derived from constructor/operation names via hidden name transformations.
      -- Note: While some are new constructor names (Hnd, Op, Ops), others are identifiers derived from constructor names (tag, val, op, etc.)
      -- These patterns ensure that the hiding/transformation functions produce valid, lexable names.
      let constructorPatterns =
            [ ("@Hnd- from toHandlerConName", toHandlerConName)
            , ("@Op- from toOpConName", toOpConName)
            , ("@Ops- from toOpsConName", toOpsConName)
            , ("@tag- from toOpenTagName", toOpenTagName)
            , ("@val- from toValueOperationName", toValueOperationName)
            , ("@extern- from newHiddenExternalName", newHiddenExternalName)
            , ("@create- from newCreatorName", newCreatorName)
            , ("@op- from toOpTypeName", toOpTypeName)
            , ("@singleton- from makeHiddenName \"singleton\"", makeHiddenName "singleton")
            ]
          testRoundTrip label op nameStr =
            it (label ++ " with " ++ nameStr) $ do
              let n1 = readQualifiedName nameStr
                  n2 = op n1
                  s = show (prettyName defaultColorScheme n2)
              name <- testLex s
              fmap showPlain name `shouldBe` Just (showPlain n2)

      forM_ constructorPatterns $ \(label, op) -> do
        testRoundTrip label op "Foo"
        testRoundTrip label op "Cons"
        testRoundTrip label op "std/core/List"

    describe "isSymbolName preservation" $ do
      let checkSymbolName op nameStr =
            it ("preserves isSymbolName for " ++ nameStr) $ do
              let n1 = newName nameStr
              let n2 = op n1
              isSymbolName n1 `shouldBe` True
              isSymbolName n2 `shouldBe` True

              let s = show (prettyName defaultColorScheme n2)
              let input = BC.pack s
              let res = lexing (R.Source "test" input) 1 input
              case res of
                (Lexeme _ (LexIdOp _) : _) -> return ()
                _ -> expectationFailure $ "Expected LexIdOp but got " ++ show res

      checkSymbolName (makeHiddenName "lift") "|->"
      checkSymbolName (toUniqueName 1) "/"
      checkSymbolName (makeHiddenName "op") "+"

    describe "Primed identifier tests" $ do
      it "classifies t' as a non-symbol name" $ do
        isSymbolName (newName "t'") `shouldBe` False
      it "classifies t'' as a non-symbol name" $ do
        isSymbolName (newName "t''") `shouldBe` False
      it "classifies r'' as a non-symbol name" $ do
        isSymbolName (newName "r''") `shouldBe` False
      it "round-trips primed names through pretty/lex" $ do
        let n1 = newName "t''"
            s = show (prettyName defaultColorScheme n1)
        s `shouldBe` "t''"
        name <- testLex s
        fmap showPlain name `shouldBe` Just (showPlain n1)

    describe "Issue 794 regression test" $ do
      it "handles the specific name from the issue (@mlift-@x10003/)" $ do
        let n1 = newName "/"
        let n2 = toUniqueName 10003 n1
        let n3 = makeHiddenName "mlift" n2

        showPlain n3 `shouldBe` "@mlift-@x10003/"
        isSymbolName n3 `shouldBe` True
        show (prettyName defaultColorScheme n3) `shouldBe` "(@mlift-@x10003/)"

        let s = show (prettyName defaultColorScheme n3)
        name <- testLex s
        fmap showPlain name `shouldBe` Just (showPlain n3)
