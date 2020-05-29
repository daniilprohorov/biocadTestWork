{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS -fno-warn-overlapping-patterns -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import           Database.Bolt

import           Control.Monad
import           Data.Default
import           Prelude                       hiding (id)
import           Text.InterpolatedString.Perl6 (qq)

import           Convert
import           Create
import           Reaction
import           Types

import           Data.Either
import           Database.Bolt.Serialization


-- generate reactions with semi-automatic methods
main :: IO ()
main = do pipe <- connect $ def { user = "neo4j", password = "12345" }
          let
              reactions =
                  [ createReactionGen 1  [1, 2]            [1]     [3, 4]
                  , createReactionGen 2  [3, 4, 5]         [2, 3]  [6, 7]
                  , createReactionGen 3  [5, 8]            [4]     [9, 10]
                  , createReactionGen 4  [7, 9]            [5]     [11, 12]
                  , createReactionGen 5  [6, 12]           [6]     [13]
                  , createReactionGen 6  [11, 12]          [7]     [14, 15]
                  , createReactionGen 7  [10, 11]          [8]     [16]
                  , createReactionGen 8  [13, 14, 15, 16]  [9, 10] [17]
                  , createReactionGen 9  [8, 10]           [11]    [18]
                  , createReactionGen 10 [10, 18]          [12]    [19, 20]
                  , createReactionGen 11 [19, 20]          [13]    [16, 17]
                  ]
          _ <- run pipe $ mapM create reactions

          reactionV <- run pipe $ getReactionGen 8
          print reactionV

          path <- run pipe $ findPath 1 17

          print path

          close pipe


-- example of generation one reaction
-- main :: IO ()
-- main = do
--     pipe <- connect $ def { user = "neo4j", password = "12345" }
--     let
--         -- create molecules in Haskell
--         m1 = Molecule { id = 1, smiles = "m1", iupacName = "m1" }
--         m2 = Molecule { id = 2, smiles = "m2", iupacName = "m2" }
--         m3 = Molecule { id = 3, smiles = "m3", iupacName = "m3" }
--         m4 = Molecule { id = 4, smiles = "m4", iupacName = "m4" }

--         -- create catalyst
--         c = Catalyst { id = 1, smiles = "catalyst", maybeName = Nothing }

--         -- create accelerate
--         acc = ACCELERATE { temperature = 211.99, pressure = 1.98878 }

--         -- create product_from for each output molecule
--         pf1 = PRODUCT_FROM { amount = 0.33 }
--         pf2 = PRODUCT_FROM { amount = 0.66 }

--         -- create reaction field
--         r1 = Reaction {id = 1, name = "m1 + m2 + c = m3 + m4"}

--         -- and at the end we can
--         -- create reaction that takes m1 and m2 moleculas + catalyst and return m3 and m4 moleculas
--         reaction = ReactionGen
--             { reaction     = r1
--             , moleculasIn  = [m1, m2]
--             , catalysts    = [(c, acc)]
--             , moleculasOut = [(m3, pf1), (m4, pf2)]
--             }

--     _ <- run pipe $ do
--         create reaction

--     reactionV <- run pipe $ getReactionGen 1
--     -- now reactionV has internal Haskell type as when we created it (Reaction)
--     print reactionV
--     close pipe


-- find path between two moleculas by id
findPath :: Int -> Int -> BoltActionT IO [DataNode]
findPath idS idE= do
    records <- query [qq|MATCH p=shortestPath((m1:Molecule \{id:$idS\})-[:PRODUCT_FROM|:REAGENT_IN*1..100]->(m2:Molecule \{id:$idE\})) return nodes(p)|]
    values <- forM records $ (\record -> record `at` "nodes(p)")
    let
        structures = map fromS $ concatMap fromL values
        eitherNodes :: [Either UnpackError Node]
        eitherNodes = map fromStructure structures
        nodes = case partitionEithers eitherNodes of
            ([], v) -> v
            _       -> error "UnpackError"
        output = map toData nodes

    return output
