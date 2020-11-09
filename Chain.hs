module Chain where

import Numeric.Natural
import Control.Monad
import Data.Foldable
import qualified Data.Map as M


type Address = String
type Hash = String

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
  , algo      :: String      -- one way function Block -> Hash
  , nonce     :: Nonce
} deriving (Show)

type Chain = [Block]         -- reverse chronological order
type Height = Natural
type Scale = Block -> Maybe Height
type Balance = M.Map Address Natural

toNatural :: Integer -> Maybe Natural
toNatural i = if i >= 0 then Just (fromIntegral i) else Nothing

update :: (Address, Integer) -> Balance -> Maybe Balance
update (addr, change) bal = 
  let newVal = change + (fromIntegral $ M.findWithDefault 0 addr bal) in
    liftM (\x -> M.insert addr x bal) $ toNatural newVal

-- app = flip . foldr 
appM = flip . foldrM 

addEntry :: Entry -> Balance -> Maybe Balance
addEntry e = update (address e, change e)

addTx :: Tx -> Balance -> Maybe Balance
addTx tx = appM addEntry (entries tx)

addBlock :: Block -> Balance -> Maybe Balance
addBlock b = addEntry (reward b) <=< appM addTx (txs b)


acc :: Scale -> Block -> (Height, Balance) -> Maybe (Height, Balance)
acc scale b (total, bal) = liftM2 (,)
   (liftM (total +) (scale b))
   (addBlock b bal)

eval :: Scale -> Chain -> (Height, Balance) -> Maybe (Height, Balance)
eval scale = appM (acc scale) 

fee :: Tx -> Integer
fee = sum . (map $ negate . change) . entries

blockReward :: Block -> Integer
blockReward b = (change $ reward b) - (sum $ map fee $ txs b)




