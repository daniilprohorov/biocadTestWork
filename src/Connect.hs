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

class Connect a b c where
    bind :: a -> b -> c -> BoltActionT IO [Record]

instance Connect Molecule Reaction REAGENT_IN where
    bind (Molecule idM smiles iupacName) (Reaction idR name) REAGENT_IN = query
        [qq|MERGE (m:Molecule \{id: $idM, smiles: "$smiles", iupacName: "$iupacName"\})
            MERGE (r:Reaction \{id: $idR, name: "$name"\})
            MERGE (m)-[:REAGENT_IN]->(r)
           |]

instance Connect Reaction Molecule PRODUCT_FROM where
    bind (Reaction idR name) (Molecule idM smiles iupacName) (PRODUCT_FROM amount) = query
        [qq|MERGE (r:Reaction \{id: $idR, name: "$name"\})
            MERGE (m:Molecule \{id: $idM, smiles: "$smiles", iupacName: "$iupacName"\})
            MERGE (r)-[:PRODUCT_FROM \{amount: {show amount}\}]->(m)
           |]

instance Connect Catalyst Reaction ACCELERATE where
    bind (Catalyst idC smiles nameC) (Reaction idR nameR) (ACCELERATE tmp prs)= query
        [qq|MERGE (c:Catalyst \{id: $idC, smiles: "$smiles", name: "$nameCV"\})
            MERGE (r:Reaction \{id: $idR, name: "$nameR"\})
            MERGE (c)-[:ACCELERATE \{temperature: {show tmp}, pressure: {show prs}\}]->(r)
           |]
            where
                nameCV = case nameC of
                    Just v  -> v
                    Nothing -> ""
