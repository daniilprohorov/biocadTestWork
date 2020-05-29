{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS -fno-warn-overlapping-patterns -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-cse #-}

module Connect
    ( bind
    ) where

import           Database.Bolt
import           Text.InterpolatedString.Perl6 (qq)
import           Types

bind :: DataNode -> DataNode -> DataEdge -> BoltActionT IO [Record]
bind (Molecule idM smiles iupacName) (Reaction idR name) REAGENT_IN = query
    [qq|MERGE (m:Molecule \{id: $idM, smiles: "$smiles", iupacName: "$iupacName"\})
        MERGE (r:Reaction \{id: $idR, name: "$name"\})
        MERGE (m)-[:REAGENT_IN]->(r)
        |]

bind (Reaction idR name) (Molecule idM smiles iupacName) (PRODUCT_FROM amount) = query
    [qq|MERGE (r:Reaction \{id: $idR, name: "$name"\})
        MERGE (m:Molecule \{id: $idM, smiles: "$smiles", iupacName: "$iupacName"\})
        MERGE (r)-[:PRODUCT_FROM \{amount: {show amount}\}]->(m)
        |]

bind (Catalyst idC smiles nameC) (Reaction idR nameR) (ACCELERATE tmp prs)= query
    [qq|MERGE (c:Catalyst \{id: $idC, smiles: "$smiles", name: "$nameCV"\})
        MERGE (r:Reaction \{id: $idR, name: "$nameR"\})
        MERGE (c)-[:ACCELERATE \{temperature: {show tmp}, pressure: {show prs}\}]->(r)
        |]
        where
            nameCV = case nameC of
                Just v  -> v
                Nothing -> ""

bind _ _ _ = error "Pattern mathing bind error"
