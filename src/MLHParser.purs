module MLHParser where

import Prelude hiding (between)

import Control.Alternative ((<|>))
import Data.Argonaut as A
import Data.Array (fromFoldable)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, intercalate)
import Data.List ((:))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.Number.Format as Math
import Data.String (Pattern(..))
import Data.String.CodeUnits (fromCharArray, stripSuffix)
import Data.Traversable (traverse)
import Effect.Aff (Aff, error, throwError)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)
import Parsing (ParserT, fail, runParser)
import Parsing.Combinators (between, endBy1, many, manyTill, sepBy1, try)
import Parsing.Combinators.Array (many1)
import Parsing.String (anyChar, char, satisfy, string)
import Parsing.String.Basic (alphaNum, intDecimal, letter, noneOf, number, oneOf, skipSpaces, whiteSpace)

--------------------------------------------------------------------------------
-- | Parsing MLH expressions
--------------------------------------------------------------------------------

data Value
  = VInt Int
  | VString String
  | VBool Boolean
  | VDecimal Number

derive instance Eq Value

instance Show Value where
  show = case _ of
    VInt int -> "VInt " <> show int
    VString string -> "VString " <> show string
    VBool bool -> "VBool " <> show bool
    VDecimal d -> "VDecimal " <> show d

instance A.EncodeJson Value where
  encodeJson = case _ of
    VInt int -> A.encodeJson int
    VString string -> A.encodeJson string
    VBool bool -> A.encodeJson bool
    VDecimal d -> A.encodeJson d

parseValue :: forall m. Monad m => ParserT String m Value
parseValue =
  parseBool
    <|> VInt <$> intDecimal
    <|> VString <$> parseString
    <|> VDecimal <$> number
  where
  parseBool =
    string "true" *> pure (VBool true) <|>
      string "false" *> pure (VBool false)

newtype Ident = Ident String

derive instance Eq Ident
derive newtype instance Show Ident

parseIdent :: forall m. Monad m => ParserT String m Ident
parseIdent =
  let
    identStart = letter <|> oneOf [ '_', '$' ]
    identLetter = alphaNum <|> oneOf [ '_', '$' ]
  in
    Ident <<< fromCharArray <<< fromFoldable <$> ((:) <$> identStart <*> many identLetter)

newtype MLHFilePath = MLHFilePath (NonEmptyList String)

derive instance Eq MLHFilePath
derive newtype instance Show MLHFilePath

asFilePath :: MLHFilePath -> String
asFilePath (MLHFilePath components) = intercalate "/" (NonEmptyList.toList components) <> ".mlh"

parseMLHFilePath :: forall m. Monad m => ParserT String m MLHFilePath
parseMLHFilePath = MLHFilePath <$> do
  -- The ']' character is the closing delimiter for an mlh expression, so it can't appear in the path.
  -- mlh requires filepaths to be enclosed in double quotes, so ideally no one is naming files
  -- with double quotes in them.
  let pathChar = noneOf [ '/', ']', '\"' ]
  components <- map mkString <$>
    between (char '\"') (char '\"') (char '/' *> sepBy1 (many1 pathChar) (char '/'))
  let { init, last } = NonEmptyList.unsnoc components
  case stripSuffix (Pattern ".mlh") last of
    Just fn' -> pure $ NonEmptyList.snoc' init fn'
    Nothing -> fail "Expected a .mlh file extension"

data Statement
  = StmtDef Ident Value
  | StmtUndef Ident
  | StmtImport MLHFilePath
  | StmtComment String
  | StmtBlock (Array Statement)

derive instance Eq Statement
instance Show Statement where
  show = formatStatement

parseStatement :: forall m. Monad m => ParserT String m Statement
parseStatement =
  let
    parseDef = do
      void $ string "%%define"
      skipSpaces
      ident <- parseIdent
      skipSpaces
      value <- parseValue
      pure (StmtDef ident value)
    parseUndef = do
      void $ string "%%undef"
      skipSpaces
      ident <- parseIdent
      pure (StmtUndef ident)
    parseImport = do
      void $ string "%%import"
      skipSpaces
      path <- parseMLHFilePath
      pure (StmtImport path)
    parseComment = do
      void $ string "(*"
      comment <- manyTill anyChar (string "*)")
      pure (StmtComment <<< fromCharArray <<< fromFoldable $ comment)
  in
    between (char '[') (char ']') (skipSpaces *> (parseDef <|> parseUndef <|> parseImport) <* skipSpaces)
      <|> parseComment

parseString :: forall m. Monad m => ParserT String m String
parseString = mkString <$> between (char '\"') (char '\"') (many (try escaped <|> normalChar))
  where
  escaped = '\"' <$ string "\\\""
  normalChar = satisfy ((/=) '\"')

mkString :: forall f. Foldable f => f Char -> String
mkString = fromCharArray <<< fromFoldable

parseStatements :: forall m. Monad m => ParserT String m (NonEmptyList Statement)
parseStatements = endBy1 parseStatement whiteSpace

--------------------------------------------------------------------------------
-- | Expanding imported mlh files
--------------------------------------------------------------------------------

expandStatement :: Statement -> Aff Statement
expandStatement stmt = case stmt of
  StmtImport path -> do 
    s <- expandImports path
    pure $ StmtBlock $ 
      Array.cons (StmtComment ("BEGIN " <> asFilePath path)) s `Array.snoc` StmtComment ("END " <> asFilePath path)
  _ -> pure stmt 

expandImports :: MLHFilePath -> Aff (Array Statement)
expandImports path = do
  log $ "Importing " <> asFilePath path
  contents <- readTextFile UTF8 (asFilePath path)
  case runParser contents parseStatements of
    Right stmts -> traverse expandStatement (fromFoldable stmts)
    Left err -> throwError $ error (show err)

formatStatement :: Statement -> String
formatStatement stmt = case stmt of
  StmtDef (Ident ident) value -> "[%%define " <> ident <> " " <> formatValue value <> "]"
  StmtUndef (Ident ident) -> "[%%undef " <> ident <> "]"
  StmtImport path -> "[%%import \"" <> asFilePath path <> "\"]"
  StmtComment comment -> "(*" <> comment <> "*)"
  StmtBlock stmts -> "\n" <> intercalate "\n" (fromFoldable (formatStatement <$> stmts)) <> "\n"
  where
  formatValue = case _ of
    VInt i -> show i
    VString s -> "\"" <> s <> "\""
    VBool b -> if b then "true" else "false"
    VDecimal d -> Math.toString d


parseRootFile :: FilePath -> Aff (Array Statement)
parseRootFile path = do
  log $ "Importing " <> path
  contents <- readTextFile UTF8 path
  let eStatments = runParser contents parseStatements
  either (throwError <<< error <<< show) (pure <<< fromFoldable) eStatments

writeSimpleMLH :: FilePath -> Array Statement -> Aff Unit
writeSimpleMLH outFile statements = do
  statements' <- traverse expandStatement statements
  let formatted = formatStatement <$> statements'
  writeTextFile UTF8 outFile (intercalate "\n" (fromFoldable formatted))