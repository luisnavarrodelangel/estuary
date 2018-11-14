{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.TextNotation where

import Text.JSON
import Text.JSON.Generic

import Estuary.Types.TidalParser

data TextNotation =
  TidalTextNotation TidalParser |
  Punctual
  deriving (Read,Eq,Ord,Data,Typeable)

instance Show TextNotation where
  show (TidalTextNotation x) = show x
  show Punctual = "Punctual"

instance JSON TextNotation where
  showJSON = toJSON
  readJSON = fromJSON