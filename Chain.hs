module Chain where

import Numeric.Natural
import Control.Monad
import Data.Foldable
import qualified Data.Map as M


type Address = String
type Hash = String
type CoinAmt = Natural

-- ledger entry
data Entry = Entry {
  parent    :: Hash
  , address :: Address
  , change  :: Integer
} deriving (Show)

-- transaction
data Tx = Tx {
  header      :: String
  , entries   :: [Entry]
} deriving (Show)

type Nonce = String

data Block = Block {
  txs         :: [Tx]
  , reward    :: Entry    
  , algo      :: String       -- one way function Block -> Hash
  , nonce     :: Nonce        
} deriving (Show)             

-- Idea: make "algo" a map from a fixed parameter space to a fixed function space
-- Use the hash as a seed for a dataset generator (or dataset constraint)
-- Hash -> Dataset (preagreed)
-- Acceptance criterion (function with at most specified loss)
-- BTC : ByteString must have specified format (from tx pool) except for nonce
-- Acceptance criterion: n-projection to 0 has 0 loss

type Chain = [Block]         -- reverse chronological order
type Weight = Natural
type Scale = Block -> Maybe Weight
type Parser = String -> Maybe (Block -> Hash)
type Balance = M.Map Address CoinAmt
type BlockDB = M.Map Hash Block

toNatural :: Integer -> Maybe Natural
toNatural i = if i >= 0 then Just (fromIntegral i) else Nothing

update :: (Address, Integer) -> Balance -> Maybe Balance
update (addr, change) bal = 
  let newVal = change + (fromIntegral $ M.findWithDefault 0 addr bal) in
    liftM (\x -> M.insert addr x bal) $ toNatural newVal

appM = flip . foldrM 

addEntry :: Entry -> Balance -> Maybe Balance
addEntry e = update (address e, change e)

addTx :: Tx -> Balance -> Maybe Balance
addTx tx = appM addEntry (entries tx)

addBlock :: Block -> Balance -> Maybe Balance
addBlock b = addEntry (reward b) <=< appM addTx (txs b)

parents :: Block -> [Hash]
parents b = (parent $ reward b) : (map parent $ concat $ map entries $ txs b)

acc :: Parser -> Scale -> Block -> (Hash, Weight, Balance)
  -> Maybe (Hash, Weight, Balance)
acc parse scale b (hash, total, bal) = liftM3 (,,)
   (if hash `elem` parents b
    then ap (parse $ algo b) (pure b)
    else Nothing)
   (liftM (total +) (scale b))
   (addBlock b bal)

eval :: Parser -> Scale -> Chain -> (Hash, Weight, Balance)
  -> Maybe (Hash, Weight, Balance)
eval parse scale = appM (acc parse scale) 

fee :: Tx -> Integer
fee = sum . (map $ negate . change) . entries

blockReward :: Block -> Integer
blockReward b = (change $ reward b) - (sum $ map fee $ txs b)





