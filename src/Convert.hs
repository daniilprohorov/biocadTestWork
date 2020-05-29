
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS -fno-warn-overlapping-patterns -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-cse #-}

module Convert
    ( Convert
    , toData
    , fromT
    , fromI
    , fromF
    ) where

import           Database.Bolt

import qualified Data.Map      as M
import           Data.Text     hiding (concat, head, length, map, replicate,
                                zip)

import           Types

class Convert a b where
    toData :: a -> b

instance Convert Node Molecule where
    toData Node {labels=["Molecule"], nodeProps} = Molecule { id = idV, iupacName = iupacNameV, smiles = smilesV }
        where
          idV = fromI $ nodeProps M.! "id"
          iupacNameV = fromT $ nodeProps M.! "iupacName"
          smilesV = fromT $ nodeProps M.! "smiles"
    toData _ = error "toData Node Molecule pattern match error"

instance Convert Node Reaction where
    toData Node {labels=["Reaction"], nodeProps} = Reaction { id = idV, name = nameV }
        where
          idV = fromI $ nodeProps M.! "id"
          nameV = fromT $ nodeProps M.! "name"
    toData _ = error "toData Node Reaction pattern match error"

instance Convert Node Catalyst where
    toData Node {labels=["Catalyst"], nodeProps} = Catalyst { id = idV, smiles = smilesV, name = nameV }
        where
          idV = fromI $ nodeProps M.! "id"
          smilesV = fromT $ nodeProps M.! "smiles"
          nameV' = fromT $ nodeProps M.! "name"
          nameV = case nameV' of
              "" -> Nothing
              v  -> Just v
    toData _ = error "toData Node Catalyst pattern match error"

instance Convert Relationship PRODUCT_FROM where
    toData Relationship {relType="PRODUCT_FROM", relProps} = PRODUCT_FROM { amount = amountV }
        where
          amountV = fromF $ relProps M.! "amount"
    toData _ = error "toData Relationship PRODUCT_FROM pattern match error"

instance Convert Relationship ACCELERATE where
    toData Relationship {relType="ACCELERATE", relProps} = ACCELERATE { temperature = tempV, pressure = pressV }
        where
          tempV = fromF $ relProps M.! "temperature"
          pressV = fromF $ relProps M.! "pressure"
    toData _ = error "toData Relationship ACCELERATE pattern match error"

fromI :: Value -> Int
fromI (I v) = v
fromI _     = error "Value is not I _ "

fromT :: Value -> Text
fromT (T v) = v
fromT _     = error "Value is not T _ "

-- F is Double?!?!, ok...
fromF :: Value -> Double
fromF (F v) = v
fromF _     = error "Value is not F _ "
