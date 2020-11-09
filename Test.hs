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

parse :: M.Map String (Block -> Hash)
parse = M.fromList [(test_algo1_src, test_algo1), (test_algo2_src, test_algo2)]


test_scale :: Scale
test_scale b = do
  owf <- M.lookup (algo b) parse
  rwdF <- toNatural $ 10 - blockReward b
  hashF <- pure $ fromIntegral $ length $ filter (== '0') $ owf b
  pure $ hashF * rwdF


test_bal :: Balance
test_bal = M.fromList $ zip ["alice", "bob", "fred", "edna"] $ repeat 100

test_entry = Entry "" "alice" (-20)

test_tx = Tx {
  header = "Hello, world"
  , entries = [Entry "" "alice" (-20), Entry "" "louis" 10]
}

test_block = Block {
   txs = [test_tx]
   , reward = Entry "" "fred" 1
   , algo = test_algo1_src
   , nonce = "test_nonce"
}

test_chain :: Chain
test_chain = [test_block]

test_addBlock = addBlock test_block test_bal

-- initial conditions
test_ic = (0, test_bal)

test_eval = eval test_scale test_chain test_ic
test_hash = test_algo1 test_block

test_tx2 = Tx {
  header = "Transaction 2"
  , entries = [Entry test_hash "alice" (-20), Entry test_hash "louis" 10]
}
  
test_block2 = Block {
   txs = [test_tx2]
   , reward = Entry test_hash "fred" 2
   , algo = test_algo2_src
   , nonce = "test_nonce2"
}

test_chain2 :: Chain
test_chain2 = [test_block2, test_block]
test_hash2 = test_algo2 test_block2
test_eval2 = eval test_scale test_chain2 test_ic
