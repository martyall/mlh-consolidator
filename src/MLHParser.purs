module MLHParser where

import Prelude hiding (between)

import Control.Alternative ((<|>))
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldM, intercalate)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.CodeUnits (fromCharArray, stripSuffix)
import Debug (traceM)
import Effect.Aff (Aff, error, throwError)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
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
  traceM $ show components
  let { init, last } = NonEmptyList.unsnoc components
  case stripSuffix (Pattern ".mlh") last of
    Just fn' -> pure $ NonEmptyList.snoc' init fn'
    Nothing -> do
      traceM "oh shit"
      fail "Expected a .mlh file extension"

data Statement
  = StmtDef Ident Value
  | StmtUndef Ident
  | StmtImport MLHFilePath
  | StmtComment String

derive instance Eq Statement

instance Show Statement where
  show = case _ of
    StmtDef ident value -> "StmtDef " <> show ident <> " " <> show value
    StmtUndef ident -> "StmtUndef " <> show ident
    StmtImport path -> "StmtImport " <> show path
    StmtComment comment -> "StmtComment " <> show comment

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
      traceM "Parsing import statement"
      skipSpaces
      traceM "SkippedSpace"
      path <- parseMLHFilePath
      traceM $ "Parsed path: " <> show path
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
-- | Parsing MLH from an imported file
--------------------------------------------------------------------------------

data Config
  = CVal Value
  | CObj (Map String Config)

derive instance Eq Config
instance Show Config where
  show = case _ of
    CVal value -> show value
    CObj obj -> show obj

parseConfig :: Array Statement -> Aff Config
parseConfig statements =
  let
    f :: Config -> Statement -> Aff Config
    f (CObj obj) stmt = case stmt of
      StmtDef (Ident ident) value ->
        pure $ CObj $ Map.insert ident (CVal value) obj
      StmtUndef (Ident _) ->
        --pure $ CObj $ Map.delete ident obj
        pure $ CObj obj
      StmtImport path@(MLHFilePath p) -> do
        log $ "Importing " <> asFilePath path
        contents <- readTextFile UTF8 (asFilePath path)
        case runParser contents parseStatements of
          Right stmts -> do
            cfg' <- parseConfig (fromFoldable stmts)
            pure $ nestObject obj p cfg'
          Left err -> throwError $ error (show err)
      StmtComment _ ->
        pure $ CObj obj
    f _ _ = throwError $ error "Expected an object"
  in
    foldM f (CObj Map.empty) statements
  where
  nestObject :: Map String Config -> NonEmptyList String -> Config -> Config
  nestObject obj keys value = case NonEmptyList.uncons keys of
    { head: key, tail: Nil } -> CObj $ Map.insert key value obj
    { head: key, tail: (Cons k ks) } ->
      let
        nested = nestObject obj (NonEmptyList.cons' k ks) value
      in
        CObj $ Map.insert key nested obj

parseRootFile :: FilePath -> Aff Config
parseRootFile path = do
  log $ "Importing " <> path
  contents <- readTextFile UTF8 path
  case runParser contents parseStatements of
    Right stmts -> parseConfig (fromFoldable stmts)
    Left err -> throwError $ error (show err)

