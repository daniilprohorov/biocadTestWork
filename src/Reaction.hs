{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS -fno-warn-overlapping-patterns -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-cse #-}

module Reaction
    ( getReactionGen
    , createReactionGen
    ) where

import           Control.Monad
import           Data.Text                     hiding (map, zip, replicate)
import           Database.Bolt
import           Prelude                       hiding (id)
import           Text.InterpolatedString.Perl6 (qq)
import           Types
import           Convert


-- take reaction by id
getReactionGen :: Int -> BoltActionT IO ReactionGen
getReactionGen id = do
    react    <- reaction id
    mInData  <- moleculasIn id
    mOutData <- moleculasOut id
    cat      <- catalysts id
    acc      <- accelerate id
    pf       <- product_from id
    let
        reactV = case react of
            [x]   -> x
            (x:_) -> x -- strange situation
            _     -> error "Can not find this reaction" -- Not the best, but for test work seems ok
        mInV = mInData
        catV = zip cat acc
        mOutV = zip mOutData pf

    return $ ReactionGen reactV mInV catV mOutV
    where
        moleculasIn :: Int -> BoltActionT IO [DataNode]
        moleculasIn idV = liftM (map toData) $ do
          records <- query [qq|MATCH (r:Reaction \{id:$idV\}) MATCH (r)-[p:REAGENT_IN]-(m:Molecule) RETURN m|]
          forM records $ (\record -> record `at` "m") :: BoltActionT IO [Node]

        moleculasOut :: Int -> BoltActionT IO [DataNode]
        moleculasOut idV = liftM (map toData) $ do
          records <- query [qq|MATCH (r:Reaction \{id:$idV\}) MATCH (r)-[p:PRODUCT_FROM]-(m:Molecule) RETURN m|]
          forM records $ (\record -> record `at` "m") :: BoltActionT IO [Node]

        product_from :: Int -> BoltActionT IO [DataEdge]
        product_from idV = liftM (map toData) $ do
          records <- query [qq|MATCH (r:Reaction \{id:$idV\}) MATCH (r)-[p:PRODUCT_FROM]-(m:Molecule) RETURN p|]
          forM records $ (\record -> record `at` "p") :: BoltActionT IO [Relationship]

        accelerate :: Int -> BoltActionT IO [DataEdge]
        accelerate idV = liftM (map toData) $ do
          records <- query [qq|MATCH (r:Reaction \{id:$idV\}) MATCH (c:Catalyst)-[p:ACCELERATE]-(r) RETURN p|]
          forM records $ (\record -> record `at` "p") :: BoltActionT IO [Relationship]

        catalysts :: Int -> BoltActionT IO [DataNode]
        catalysts idV = liftM (map toData) $ do
          records <- query [qq|MATCH (r:Reaction \{id:$idV\}) MATCH (r)-[p:ACCELERATE]-(c:Catalyst) RETURN c|]
          forM records $ (\record -> record `at` "c") :: BoltActionT IO [Node]

        reaction :: Int -> BoltActionT IO [DataNode]
        reaction idV = liftM (map toData) $ do
          records <- query [qq|MATCH (r:Reaction \{id:$idV\}) RETURN r|]
          forM records $ (\record -> record `at` "r") :: BoltActionT IO [Node]


moleculeCreate n = map (\i -> Molecule i (pack $ "m" ++ show i) (pack $ "m" ++ show i)) [1..n]
reactionCreate n = map (\i -> Reaction i (pack $ "r" ++ show i) ) [1..n]
catalystCreate n = map (\i -> Catalyst i (pack $ "c" ++ show i) (if i `mod` 2 == 0 then Just (pack ("c" ++ show i)) else Nothing) ) [1..n]
accelerationCreate n = replicate n (ACCELERATE 0.5 0.5)


createReactionGen rId mInIds cIds mOutIds = ReactionGen reactV mInV catV mOutV
    where
        m = moleculeCreate 20
        c = zip (catalystCreate 13) (accelerationCreate 20)
        r = reactionCreate 11
        p = PRODUCT_FROM 0.9
        get lst n = lst !! (n-1)
        reactV = r `get` rId
        mInV = map (\i -> m `get` i) mInIds
        catV = map (\i -> c `get` i) cIds
        mOutV = zip (map (\i -> m `get` i) mOutIds) (repeat p)
