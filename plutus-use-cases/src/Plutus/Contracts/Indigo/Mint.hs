{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Plutus.Contracts.Indigo.Mint(
    MintSchema
    , MintError(..)
    , RegisterAsset(..)
    , OpenPosition(..)
    , mint
    ) where

import           Control.Monad                (forever)
import           Data.Aeson                   (FromJSON, ToJSON)
import           GHC.Generics                 (Generic)
import           Plutus.Contract
import           Plutus.Contract.StateMachine (SMContractError)
import           Schema                       (ToSchema)

data MintError = MintContractError ContractError | MintSMError SMContractError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- TODO: Better types here!
type Decimal = Double
type Uint128 = Integer
type HumanAddr = String

data AssetInfo = AssetInfo
  { contract_token_addr :: HumanAddr
  , native_token_denom  :: String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Asset = Asset
  { info   :: AssetInfo
  , amount :: Uint128
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RegisterAsset = RegisterAsset
  { asset_token          :: HumanAddr
  , auction_discount     :: Decimal
  , min_collateral_ratio :: Decimal
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OpenPosition = OpenPosition
  { asset_info       :: AssetInfo
  , collateral       :: Asset
  , collateral_ratio :: Decimal
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type MintSchema =
  BlockchainActions
    .\/ Endpoint "RegisterAsset" RegisterAsset
    .\/ Endpoint "OpenPosition" OpenPosition

mint ::
  ( HasBlockchainActions s
  , HasEndpoint "RegisterAsset" RegisterAsset s
  , HasEndpoint "OpenPosition" OpenPosition s
  )
  => Contract w s MintError ()
mint = forever (registerAsset `select` openPosition)

registerAsset ::
  ( HasBlockchainActions s
  , HasEndpoint "RegisterAsset" RegisterAsset s
  )
  => Contract w s MintError ()
registerAsset = do
  regAsset <- mapError MintContractError $ endpoint @"RegisterAsset"
  logInfo @String (show regAsset)

openPosition ::
  ( HasBlockchainActions s
  , HasEndpoint "OpenPosition" OpenPosition s
  )
  => Contract w s MintError ()
openPosition = do
  openPos <- mapError MintContractError $ endpoint @"OpenPosition"
  logInfo @String (show openPos)
