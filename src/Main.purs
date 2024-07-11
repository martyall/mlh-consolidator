module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

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

main :: Effect Unit
main = do
  log "🍝"

data Value = VInt Int | VString String | VBool Boolean

data Statement
  = StmtDef String Value
  | StmtUndef String
  | StmtImport String
  | Comment String

