module Chain where
  import Numeric.Natural
  import Control.Monad
  import Data.Foldable
  import qualified Data.Map as M


  type Address = String
  type CoinAmt = Natural
  type Balance = M.Map Address CoinAmt

  -- transaction
  data Tx = Tx {
     header      :: String
     , senders   :: [(Address, CoinAmt)]
     , receivers :: [(Address, CoinAmt)]
  } deriving (Show)

  type Hash = String
  type Nonce = String

  -- unsigned block
  data UBlock = UBlock {
    previous    :: Hash
    , txs       :: [Tx]
    , owf       :: SBlock -> Hash     -- one-way function
    , blkRwd    :: SBlock -> Integer  -- block reward
    , owfSrc    :: String
    , rwdSrc    :: String
  }

  -- signed block
  data SBlock = SBlock {
    ublock  :: UBlock
    , miner :: Address 
    , nonce :: Nonce
  } deriving (Show)

  type Chain = [SBlock]
  type Height = Integer
  type Scale = SBlock -> Maybe Height
 
  sent :: Tx -> CoinAmt
  sent = sum . map snd . senders

  received :: Tx -> CoinAmt
  received = sum . map snd . receivers

  -- Can be negative
  fee :: Tx -> Integer
  fee tx = fromIntegral (sent tx) - fromIntegral (received tx)

  -- Can be negative
  reward :: SBlock -> Integer
  reward sb = (blkRwd (ublock sb) sb) + (sum $ map fee $ txs $ ublock sb)

  send :: (Address, CoinAmt) -> Balance -> Maybe Balance
  send (addr, amt) bal = do
    acct <- M.lookup addr bal
    if amt < acct
    then Just $ M.adjust (\x -> x - amt) addr bal
    else Nothing

  receive :: (Address, CoinAmt) -> Balance -> Balance
  receive (addr, amt) bal =
    if M.member addr bal
    then M.adjust (+ amt) addr bal
    else M.insert addr amt bal

  app = flip . foldr 
  appM = flip . foldrM 

  addSenders :: Tx -> Balance -> Maybe Balance
  addSenders tx = appM send (senders tx)

  addReceivers :: Tx -> Balance -> Balance
  addReceivers tx = app receive (receivers tx)

  addTx :: Tx -> Balance -> Maybe Balance
  addTx tx = liftM (addReceivers tx) . addSenders tx 
  
  addUBlock :: UBlock -> Balance -> Maybe Balance
  addUBlock ub = appM addTx (txs ub)

  addReward :: SBlock -> Balance -> Maybe Balance
  addReward sb = let rwd = reward sb in
    if rwd >= 0
    then pure . receive (miner sb, fromIntegral $ rwd) 
    else send (miner sb, fromIntegral $ negate rwd) 

  addBlock :: SBlock -> Balance -> Maybe Balance
  addBlock sb = addReward sb <=< addUBlock (ublock sb) 
  

  acc :: Scale -> SBlock -> (Hash, Height, Balance) -> Maybe (Hash, Height, Balance)
  acc scale sb (hash, total, bal) = liftM3 (,,)
    (pure $ (owf (ublock sb)) sb)
    (if (previous $ ublock sb) == hash
     then liftM (total +) (scale sb)
     else Nothing
    )
    (addBlock sb bal)
    
  eval :: Scale -> Chain -> (Hash, Height, Balance) -> Maybe (Hash, Height, Balance)
  eval scale = appM (acc scale) 


  instance Show UBlock where
    show ub = "previous =  " ++ (show $ previous ub)
           ++ "txs = " ++ (show $ txs ub)
           ++ "owf = " ++ (owfSrc ub)




