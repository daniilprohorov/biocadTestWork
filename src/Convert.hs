{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS -fno-warn-overlapping-patterns -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-cse #-}

module Convert where

import           Database.Bolt
import qualified Data.Map      as M
import           Types

class Convert a b where
    toData :: a -> b

instance Convert Node DataNode where
    toData Node {labels=["Molecule"], nodeProps} = Molecule { id = idV, iupacName = iupacNameV, smiles = smilesV }
        where
          idV = fromI $ nodeProps M.! "id"
          iupacNameV = fromT $ nodeProps M.! "iupacName"
          smilesV = fromT $ nodeProps M.! "smiles"

    toData Node {labels=["Reaction"], nodeProps} = Reaction { id = idV, name = nameV }
        where
          idV = fromI $ nodeProps M.! "id"
          nameV = fromT $ nodeProps M.! "name"

    toData Node {labels=["Catalyst"], nodeProps} = Catalyst { id = idV, smiles = smilesV, maybeName = maybeNameV }
        where
          idV = fromI $ nodeProps M.! "id"
          smilesV = fromT $ nodeProps M.! "smiles"
          maybeNameV' = fromT $ nodeProps M.! "name"
          maybeNameV = case maybeNameV' of
              "" -> Nothing
              v  -> Just v
    toData _ = error "toData pattern match error"

instance Convert Relationship DataEdge where
    toData Relationship {relType="PRODUCT_FROM", relProps} = PRODUCT_FROM { amount = amountV }
        where
          amountV = fromF $ relProps M.! "amount"

    toData Relationship {relType="ACCELERATE", relProps} = ACCELERATE { temperature = tempV, pressure = pressV }
        where
          tempV = fromF $ relProps M.! "temperature"
          pressV = fromF $ relProps M.! "pressure"
    toData _ = error "toData pattern match error"


fromI (I v) = v
fromI _     = error "fromI pattern match error"

fromF (F v) = v
fromF _     = error "fromF pattern match error"

fromT (T v) = v
fromT _     = error "fromT pattern match error"

fromL (L v) = v
fromL _     = error "fromL pattern match error"

fromS (S v) = v
fromS _     = error "fromS pattern match error"
