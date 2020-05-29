{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS -fno-warn-overlapping-patterns -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-cse #-}

module Create
    ( create
    ) where

import           Connect
import           Control.Monad
import           Database.Bolt
import           Prelude                       hiding (id)
import           Text.InterpolatedString.Perl6 (qq)
import           Types


class Create a where
    create :: a -> BoltActionT IO [Record]

instance Create DataNode where
    create (Molecule id smiles iupacName) = query
        [qq|MERGE (:Molecule \{id: $id, smiles: "$smiles", iupacName: "$iupacName"\})|]
    create (Reaction id name) = query
        [qq|MERGE (:Reaction \{id: $id, name: "$name"\})|]
    create (Catalyst id smiles maybeName) = query
        [qq|MERGE (:Catalyst \{id: $id, smiles: "$smiles", name: "$maybeNameV"\})|]
            where
                maybeNameV = case maybeName of
                    Just v  -> v
                    Nothing -> ""

instance Create ReactionGen where
    create (ReactionGen reaction moleculasIn catalystsI moleculasOutI) = do
        _ <- bindManyOne moleculasIn reaction $ replicate (length moleculasIn) REAGENT_IN
        _ <- bindManyOne catalysts reaction accelerate
        bindOneMany reaction moleculasOut product_from
            where
                catalysts = map fst catalystsI
                accelerate = map snd catalystsI

                moleculasOut = map fst moleculasOutI
                product_from = map snd moleculasOutI

                bindOneMany one many param = liftM concat $
                    mapM (\(o, m, p) -> bind o m p) $ zip3 (repeat one) many param
                bindManyOne many one param = liftM concat $
                    mapM (\(m, o, p) -> bind m o p) $ zip3 many (repeat one) param
