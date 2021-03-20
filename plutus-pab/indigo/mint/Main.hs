{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import           Data.Bifunctor               (first)
import           Data.Text.Extras             (tshow)
import           Plutus.Contracts.Indigo.Mint (MintSchema, mint)
import           Plutus.PAB.ContractCLI       (commandLineApp)

main :: IO ()
main = commandLineApp $ first tshow $ mint @MintSchema @()
