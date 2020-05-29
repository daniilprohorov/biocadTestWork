{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Types where

import Data.Text


data DataNode = Molecule
    { id        :: Int
    , smiles    :: Text
    , iupacName :: Text
    } |

    Reaction
    { id   :: Int
    , name :: Text
    } |

    Catalyst
    { id     :: Int
    , smiles :: Text
    , maybeName :: Maybe Text
    } deriving (Show)

data DataEdge = PRODUCT_FROM
    { amount :: Double
    } |

    ACCELERATE
    { temperature :: Double
    , pressure    :: Double
    } |

    REAGENT_IN
    deriving (Show)


-- General type for reaction process
data ReactionGen = ReactionGen
    { reaction     :: DataNode
    , moleculasIn  :: [DataNode]
    , catalysts    :: [(DataNode, DataEdge)]
    , moleculasOut :: [(DataNode, DataEdge)]
    } deriving (Show)
