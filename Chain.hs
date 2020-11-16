module Chain where

import Numeric.Natural
import Control.Monad
import Data.Foldable
import qualified Data.Map as M


type Source a = String   -- complete description of a function 

data PublicKey  = PublicKey {
  rawPubKey      :: String
  , pkeDecrypt   :: Source (PublicKey -> String -> String)
} deriving (Show)

data Address = Address {
  rawAddr            :: String
  , pubKeyToAddr     :: Source (PublicKey -> Address)
} deriving (Show, Ord, Eq)

data Encrypted a = Encrypted {
  pubKey       :: PublicKey 
  , encrypted  :: String
} deriving (Show)

type Decrypt a = Encrypted a -> String

type Hash = String
type CoinAmt = Natural   -- coin amount
type CoinUpd = Integer   -- coin update

-- ledger entry
data Entry a = Entry {
  parent    :: Hash
  , address :: Address
  , change  :: CoinUpd
  , sig     :: Maybe (Encrypted a)
} deriving (Show)

-- transaction
data Tx = Tx {
  header        :: String
  , entries     :: [Entry Tx]
} deriving (Show)

type Nonce = String

data Block = Block {
  txs         :: [Tx]
  , reward    :: Entry Block
  , algo      :: Source (Block -> Hash)
  , nonce     :: Nonce        
} deriving (Show)             


type Chain = [Block]         -- reverse chronological order
type Weight = Natural
type Scale = Block -> Maybe Weight
type Parser a = Source a -> Maybe a
type Balance = M.Map Address CoinAmt


class (Show a) => Signable a where
  unsign :: a -> a

instance Signable (Entry a) where
  unsign e = e {sig = Nothing}

instance Signable Tx where
  unsign tx = tx { entries = map unsign (entries tx) }

instance Signable Block where
  unsign b = b { txs = map unsign (txs b)
               , reward = unsign (reward b) }


toNatural :: Integer -> Maybe Natural
toNatural i = if i >= 0 then Just (fromIntegral i) else Nothing

update :: (Address, CoinUpd) -> Balance -> Maybe Balance
update (addr, change) bal = 
  let newVal = change + (fromIntegral $ M.findWithDefault 0 addr bal) in
    liftM (\x -> M.insert addr x bal) $ toNatural newVal


validate :: (Signable a) => Parser (Decrypt a) -> Encrypted a -> a -> Bool
validate parser enc signed = case parser (pkeDecrypt $ pubKey $ enc) of
  Nothing -> False
  Just pke -> pke enc == (show $ unsign $ signed)

addEntry :: (Signable a) => Parser (Decrypt a) -> a -> Entry a
  -> Balance -> Maybe Balance
addEntry parser container entry bal = 
    if change entry < 0
    then do
      s <- sig entry
      if not $ validate parser s container
      then Nothing
      else update (address entry, change entry) bal
    else update (address entry, change entry) bal

appM = flip . foldrM

addTx :: Parser (Decrypt Tx) -> Tx -> Balance -> Maybe Balance
addTx p tx = appM (addEntry p tx) (entries tx)

addBlock :: Parser (Decrypt Tx) -> Parser (Decrypt Block)
  -> Block -> Balance -> Maybe Balance
addBlock pt pb b = addEntry pb b (reward b) <=< appM (addTx pt) (txs b)

parents :: Block -> [Hash]
parents b = (parent $ reward b) : (map parent $ concat $ map entries $ txs b)

acc :: Parser (Decrypt Tx) -> Parser (Decrypt Block)  -> Parser (Block -> Hash)
  -> Scale -> Block -> (Hash, Weight, Balance) -> Maybe (Hash, Weight, Balance)
acc pt pb parse scale b (hash, total, bal) = liftM3 (,,)
   (if hash `elem` parents b
    then ap (parse $ algo b) (pure b)
    else Nothing)
   (liftM (total +) (scale b))
   (addBlock pt pb b bal)

eval :: Parser (Decrypt Tx) -> Parser (Decrypt Block) -> Parser (Block -> Hash)
  -> Scale -> Chain -> (Hash, Weight, Balance) -> Maybe (Hash, Weight, Balance)
eval pt pb parse scale = appM (acc pt pb parse scale) 

fee :: Tx -> CoinUpd
fee = sum . (map $ negate . change) . entries

blockReward :: Block -> CoinUpd
blockReward b = (change $ reward b) - (sum $ map fee $ txs b)





