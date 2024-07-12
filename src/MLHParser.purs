module MLHParser where

import Prelude hiding (between)

import Control.Alternative ((<|>))
import Data.Argonaut as A
import Data.Array (concatMap, fromFoldable)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldM, intercalate)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number.Format as Math
import Data.Profunctor.Strong (second)
import Data.String (Pattern(..))
import Data.String.CodeUnits (fromCharArray, stripSuffix)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, error, throwError)
import Effect.Class.Console (log)
import Foreign.Object as Object
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

instance A.EncodeJson Config where
  encodeJson = case _ of
    CVal value -> A.encodeJson value
    CObj obj ->
      A.fromObject
        $ Object.fromFoldable
        $
          (map (second A.encodeJson) $ Map.toUnfoldable obj :: Array (Tuple String A.Json))

parseConfig :: Array Statement -> Aff Config
parseConfig statements =
  let
    f :: Config -> Statement -> Aff Config
    f (CObj obj) stmt = case stmt of
      StmtDef (Ident ident) value ->
        pure $ CObj $ Map.insert ident (CVal value) obj
      StmtUndef (Ident ident) ->
        pure $ CObj $ Map.delete ident obj
      StmtImport path -> do
        log $ "Importing " <> asFilePath path
        contents <- readTextFile UTF8 (asFilePath path)
        case runParser contents parseStatements of
          Right stmts -> do
            cfg' <- parseConfig (fromFoldable stmts)
            case cfg' of
              CObj obj' -> pure $ CObj $ obj `Map.union` obj'
              CVal _ -> throwError $ error "Expected an object in the config"
          Left err -> throwError $ error (show err)
      StmtComment _ ->
        pure $ CObj obj
    f _ _ = throwError $ error "Expected an object"
  in
    foldM f (CObj Map.empty) statements

-- this would be useful if we wanted to nest the config according to the directory heirachy
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

class ToSimpleMLH a where
  toSimpleMLH :: a -> Array (Tuple Ident Value)

instance ToSimpleMLH Config where
  toSimpleMLH = case _ of
    CVal value -> [ Tuple (Ident "value") value ]
    CObj obj ->
      let
        f (Tuple k v) = case v of
          CObj _ -> []
          CVal v' -> [ Tuple (Ident k) v' ]
      in
        concatMap f (Map.toUnfoldable obj)

writeSimpleMLH :: FilePath -> Config -> Aff Unit
writeSimpleMLH path config = do
  let simple = toSimpleMLH config
  let lines = map (\(Tuple (Ident k) v) -> "[%%define " <> k <> " " <> formatValue v <> "]") simple
  let contents = intercalate "\n" lines
  writeTextFile UTF8 path contents
  where
  formatValue = case _ of
    VInt i -> show i
    VString s -> "\"" <> s <> "\""
    VBool b -> if b then "true" else "false"
    VDecimal d -> Math.toString d