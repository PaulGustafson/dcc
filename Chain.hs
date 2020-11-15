module Chain where

import Numeric.Natural
import Control.Monad
import Data.Foldable
import qualified Data.Map as M


type Algo = String   -- complete description of a function 

data PublicKey = PublicKey {
  rawPubKey        :: String
  , encryptionAlgo :: Algo     -- PublicKey x EncryptedString -> DecryptedString
} deriving (Show)

data Address = Address {
  rawAddr            :: String
  , pubKeyToAddr     :: Algo      -- PublicKey -> Address
} deriving (Show, Ord, Eq)

type Hash = String
type CoinAmt = Natural   -- coin amount
type CoinUpd = Integer   -- coin update                 
type SignedString = String
type Signature = (PublicKey, SignedString)

-- ledger entry
data Entry = Entry {
  parent    :: Hash
  , address :: Address
  , change  :: CoinUpd
  , sig     :: Maybe Signature  
} deriving (Show)

-- transaction
data Tx = Tx {
  header        :: String
  , entries     :: [Entry]
} deriving (Show)

type Nonce = String

data Block = Block {
  txs         :: [Tx]
  , reward    :: Entry
  , algo      :: Algo       -- one way function Block -> Hash
  , nonce     :: Nonce        
} deriving (Show)             


type Chain = [Block]         -- reverse chronological order
type Weight = Natural
type Scale = Block -> Maybe Weight
type Parser a = Algo -> Maybe a
type Balance = M.Map Address CoinAmt
type BlockDB = M.Map Hash Block

toNatural :: Integer -> Maybe Natural
toNatural i = if i >= 0 then Just (fromIntegral i) else Nothing

update :: (Address, CoinUpd) -> Balance -> Maybe Balance
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

acc :: Parser (Block -> Hash) -> Scale -> Block -> (Hash, Weight, Balance)
  -> Maybe (Hash, Weight, Balance)
acc parse scale b (hash, total, bal) = liftM3 (,,)
   (if hash `elem` parents b
    then ap (parse $ algo b) (pure b)
    else Nothing)
   (liftM (total +) (scale b))
   (addBlock b bal)

eval :: Parser (Block -> Hash) -> Scale -> Chain -> (Hash, Weight, Balance)
  -> Maybe (Hash, Weight, Balance)
eval parse scale = appM (acc parse scale) 

fee :: Tx -> CoinUpd
fee = sum . (map $ negate . change) . entries

blockReward :: Block -> CoinUpd
blockReward b = (change $ reward b) - (sum $ map fee $ txs b)





