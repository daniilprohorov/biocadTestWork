{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Types
    ( Molecule(..)
    , Reaction(..)
    , Catalyst(..)
    , PRODUCT_FROM(..)
    , ACCELERATE(..)
    , REAGENT_IN(..)
    , ReactionGen(..)
    ) where

import Data.Text

-- Data
data Molecule = Molecule
    { id        :: Int
    , smiles    :: Text
    , iupacName :: Text
    } deriving (Show)

data Reaction = Reaction
    { id   :: Int
    , name :: Text
    } deriving (Show)

data Catalyst = Catalyst
    { id     :: Int
    , smiles :: Text
    , name   :: Maybe Text
    } deriving (Show)

-- Connections
data PRODUCT_FROM = PRODUCT_FROM
    { amount :: Double
    } deriving (Show)

data ACCELERATE = ACCELERATE
    { temperature :: Double
    , pressure    :: Double
    } deriving (Show)

data REAGENT_IN = REAGENT_IN deriving (Show)


-- General type for reaction process
data ReactionGen = ReactionGen
    { reaction     :: Reaction
    , moleculasIn  :: [Molecule]
    , catalysts    :: [(Catalyst, ACCELERATE)]
    , moleculasOut :: [(Molecule, PRODUCT_FROM)]
    } deriving (Show)
