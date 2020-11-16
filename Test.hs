module Test where

import Chain
import Data.Char
import qualified Data.Map as M
import Data.Serialize
import Crypto.Hash
import Control.Monad

test_algo1 :: Block -> Hash
test_algo1 = show . hashWith SHA256 . encode . show
test_algo1_src = "show . hashWith SHA256 . encode . show"

test_algo2 :: Block -> Hash
test_algo2 = show . hashWith SHA512 . encode . show
test_algo2_src = "show . hashWith SHA512 . encode . show"

test_parser :: Parser (Block -> Hash)
test_parser s = M.lookup s $ M.fromList [(test_algo1_src, test_algo1), (test_algo2_src, test_algo2)]

test_sig_parse :: Signable a => Parser (Decrypt a)
test_sig_parse algo = Just encrypted


test_scale :: Scale
test_scale b = do
  owf <-  test_parser (algo b)
  rwdF <- toNatural $ 10 - blockReward b
  hashF <- pure $ fromIntegral $ length $ filter (== '0') $ owf b
  pure $ hashF * rwdF


test_addr :: [Address]
test_addr =  map (flip Address "fake_addr_algo") $ ["alice", "bob", "fred", "edna"]
alice = test_addr !! 0
bob = test_addr !! 1
fred = test_addr !! 2
edna = test_addr !! 3
louis = Address "louis" "fake_addr_algo"

test_bal :: Balance
test_bal = M.fromList $ zip test_addr $ repeat 100


test_sig :: Tx -> Maybe (Encrypted a)
test_sig tx = Just $ Encrypted (PublicKey "Fake public key" "Fake PKE algo") (show tx)
           
test_entry = Entry "" (Address "alice" "fake_addr_algo") (-20) Nothing

test_tx = Tx {
  header = "Hello, world"
  , entries = [Entry "" alice (-20) Nothing, Entry "" louis 10 Nothing]
}

test_tx2 = test_tx { entries = [Entry "" alice (-20) (test_sig test_tx), Entry "" louis 10 Nothing] }

test_block = Block {
   txs = [test_tx2]
   , reward = Entry "" fred 1 Nothing
   , algo = test_algo1_src
   , nonce = "test_nonce"
}

test_chain :: Chain
test_chain = [test_block]

-- TODO
-- test_addBlock = addBlock test_block test_bal

-- -- initial conditions
-- test_ic = ("", 0, test_bal)

-- test_eval = eval test_parser test_scale test_chain test_ic
-- test_hash = test_algo1 test_block

-- test_tx2 = Tx {
--   header = "Transaction 2"
--   , entries = [Entry test_hash alice (-20) test_sig, Entry test_hash louis 10 test_sig]
-- }
  
-- test_block2 = Block {
--    txs = [test_tx2]
--    , reward = Entry test_hash fred 2 test_sig
--    , algo = test_algo2_src
--    , nonce = "test_nonce2"
-- }

-- test_chain2 :: Chain
-- test_chain2 = [test_block2, test_block]

-- test_eval2 = eval test_parser test_scale test_chain2 test_ic
