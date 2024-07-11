module MLHParser where

import Prelude hiding (between)

import Control.Alternative ((<|>))
import Data.Array (fromFoldable)
import Data.List ((:))
import Data.String.CodeUnits (fromCharArray)
import Parsing (Parser)
import Parsing.Combinators (between, many, manyTill, try)
import Parsing.Combinators.Array (many1)
import Parsing.Language (haskellStyle)
import Parsing.String (anyChar, char, satisfy, string)
import Parsing.String.Basic (intDecimal, letter, skipSpaces, whiteSpace)
import Parsing.Token (GenLanguageDef(..))

{-
[%%define ledger_depth 35]
[%%import "/src/config/curve/medium.mlh"]
[%%import "/src/config/coinbase/realistic.mlh"]
[%%import "/src/config/scan_state/point2tps.mlh"]
[%%import "/src/config/debug_level/some.mlh"]
[%%import "/src/config/proof_level/full.mlh"]
[%%import "/src/config/txpool_size.mlh"]
[%%import "/src/config/account_creation_fee/realistic.mlh"]
[%%import "/src/config/amount_defaults/realistic.mlh"]
[%%import "/src/config/protocol_version/current.mlh"]
[%%import "/src/config/supercharged_coinbase_factor/one.mlh"]

(* custom consensus parameters for the testnet release *)
[%%define k 290]
[%%define delta 0]
[%%define slots_per_epoch 7140]
[%%define slots_per_sub_window 7]
[%%define sub_windows_per_window 11]
[%%define grace_period_slots 2160]

[%%define record_async_backtraces false]
[%%define time_offsets true]
[%%define cache_exceptions false]

[%%define plugins false]

[%%define genesis_ledger "testnet_postake"]

[%%define genesis_state_timestamp "2021-09-24T00:00:00Z"]
[%%define block_window_duration 180000]

[%%define integration_tests false]
[%%define force_updates false]

[%%define download_snark_keys false]
[%%define generate_genesis_proof false]

[%%define itn_features false]

[%%define print_versioned_types false]

[%%define test_full_epoch false]
[%%import "/src/config/fork.mlh"]
[%%import "/src/config/features/public_testnet.mlh"]
(* 2*block_window_duration *)
[%%define compaction_interval 360000]
[%%define vrf_poll_interval 5000]
[%%define zkapp_cmd_limit 24]
[%%undef slot_tx_end]
[%%undef slot_chain_end]

-}

data Value 
  = VInt Int 
  | VString String 
  | VBool Boolean

derive instance Eq Value

instance Show Value where
  show = case _ of
    VInt int -> "VInt " <> show int
    VString string -> "VString " <> show string
    VBool bool -> "VBool " <> show bool

parseValue :: Parser String Value
parseValue =
  parseBool
    <|> VInt <$> intDecimal
    <|> VString <$> parseString
  where
  parseBool =
    string "true" *> pure (VBool true) <|>
      string "false" *> pure (VBool false)

newtype Ident = Ident String

derive instance Eq Ident
derive newtype instance Show Ident

parseIdent :: Parser String Ident
parseIdent =
  let
    LanguageDef { identStart, identLetter } = haskellStyle
  in
    Ident <<< fromCharArray <<< fromFoldable <$> ((:) <$> identStart <*> many identLetter)

data Statement
  = StmtDef Ident Value
  | StmtUndef Ident
  | StmtImport String
  | Comment String

derive instance Eq Statement

instance Show Statement where
  show = case _ of
    StmtDef ident value -> "StmtDef " <> show ident <> " " <> show value
    StmtUndef ident -> "StmtUndef " <> show ident
    StmtImport path -> "StmtImport " <> show path
    Comment comment -> "Comment " <> show comment


parseStatement :: Parser String Statement
parseStatement =
  let
    parseDef = do
      void $ string "%%define"
      skipSpaces
      ident <- Ident <<< fromCharArray <<< fromFoldable <$> (many1 letter)
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
      path <- parseString
      pure (StmtImport path)
    parseComment = do
      void $ string "(*"
      comment <- manyTill anyChar (string "*)")
      pure (Comment <<< fromCharArray <<< fromFoldable $ comment)
  in
    between (char '[') (char ']') (parseDef <|> parseUndef <|> parseImport)
        <|> parseComment

parseString :: Parser String String
parseString = fromCharArray <<< fromFoldable <$> between (char '\"') (char '\"') (many (try escaped <|> normalChar))
  where
  escaped = '\"' <$ string "\\\""
  normalChar = satisfy ((/=) '\"')