module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import MLHParser (Ident(..), Statement(..), Value(..), asFilePath, parseIdent, parseStatement, parseString, parseValue)
import Parsing (runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ do
  runSpec [ consoleReporter ] do 
    stringSpec
    identSpec
    valueSpec
    statementSpec


stringSpec :: Spec Unit
stringSpec =
  describe "parsing strings" do
    it "can parse the empty string" $ 
      runParser "\"\"" parseString `shouldEqual` Right ""
    it "can parse a normal string" $ 
      runParser "\"hello world\"" parseString `shouldEqual` Right "hello world"
    it "can parse a filepath" $ 
      runParser "\"../src/Main.purs\"" parseString `shouldEqual` Right "../src/Main.purs"
    it "can parse stuff with escaped strings" $ 
      runParser "\"hello \\\"world\\\"\"" parseString `shouldEqual` Right "hello \"world\""

identSpec :: Spec Unit
identSpec =
  describe "parsing ident" do
    it "can parse a simple ident" $ 
      runParser "hello" parseIdent `shouldEqual` Right (Ident "hello")
    it "can parse a complex ident" $ 
      runParser "helloWorld" parseIdent `shouldEqual` Right (Ident "helloWorld")
    it "can parse a complex ident with numbers" $ 
      runParser "helloWorld123" parseIdent `shouldEqual` Right (Ident "helloWorld123")
    it "can parse a complex ident with numbers and underscores" $ 
      runParser "hello_world_123" parseIdent `shouldEqual` Right (Ident "hello_world_123")
 
valueSpec :: Spec Unit
valueSpec = 
  describe "parsing values" do
    it "can parse a string" $ 
      runParser "\"hello world\"" parseValue `shouldEqual` Right (VString "hello world")
    it "can parse an int" $
      runParser "123" parseValue `shouldEqual` Right (VInt 123)
    it "can parse a bool" $ do
      runParser "true" parseValue `shouldEqual` Right (VBool true)
      runParser "false" parseValue `shouldEqual` Right (VBool false)

statementSpec :: Spec Unit
statementSpec = 
  describe "parsing statements" do
    it "can parse a def statement" $ 
      runParser "[%%define hello 123]" parseStatement `shouldEqual` Right (StmtDef (Ident "hello") (VInt 123))
    it "can parse an undef statement" $ 
      runParser "[%%undef hello]" parseStatement `shouldEqual` Right (StmtUndef (Ident "hello"))
    it "can parse an import statement" $ 
      case runParser "[%%import \"fixtures/cfg/medium.mlh\"]" parseStatement of
        Right (StmtImport fp) -> asFilePath fp `shouldEqual` "fixtures/cfg/medium.mlh"
        Right _ -> fail "Parser mismatch"
        Left err -> fail $ show err
    it "can parse a comment" $ 
      runParser "(* this is ( * a * comment*)" parseStatement `shouldEqual` Right (StmtComment " this is ( * a * comment")  