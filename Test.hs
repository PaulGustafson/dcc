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


test_enc :: (Signable a) => a -> Encrypted a
test_enc c = Encrypted (PublicKey "Fake public key" "Fake PKE algo") (show c)

test_sig :: (Signable a) => a -> Maybe (Encrypted a)
test_sig = Just . test_enc

           
test_entry = Entry "" (Address "alice" "fake_addr_algo") (-20) Nothing

test_tx0 = Tx {
  header = "Hello, world"
  , entries = [Entry "" alice (-20) Nothing, Entry "" louis 10 Nothing]
}

test_tx = test_tx0 { entries = [Entry "" alice (-20) (test_sig test_tx0), Entry "" louis 10 Nothing] }


test_validate_tx = validate test_sig_parse (test_enc test_tx0) (test_tx)

test_block = Block {
   txs = [test_tx]
   , reward = Entry "" fred 1 Nothing
   , algo = test_algo1_src
   , nonce = "test_nonce"
}

-- test_block = test_blockp { reward = Entry "" fred 1 (test_sig test_blockp) }

-- test_validate_blk =  validate test_sig_parse (test_enc test_block) (test_block)

test_chain :: Chain
test_chain = [test_block]

test_addBlock = addBlock test_sig_parse test_sig_parse test_block test_bal

-- initial conditions
test_ic = ("", 0, test_bal)

test_eval = eval test_sig_parse test_sig_parse test_parser test_scale test_chain test_ic
test_hash = test_algo1 test_block
  
test_tx2p = Tx {
  header = "Transaction 2"
  , entries = [Entry test_hash alice (-20) Nothing, Entry test_hash louis 10 Nothing]
}

--TODO: write signing function

test_tx2 = test_tx2p { entries = [Entry test_hash alice (-20) (test_sig test_tx2p), Entry test_hash louis 10 Nothing] }

  
test_block2p = Block {
   txs = [test_tx2]
   , reward = Entry test_hash fred 2 Nothing
   , algo = test_algo2_src
   , nonce = "test_nonce2"
}

test_block2 = test_block2p { reward = Entry test_hash fred 2 (test_sig test_block2p)}

test_chain2 :: Chain
test_chain2 = [test_block2, test_block]


assert_unsign = (show test_tx2p) == (show $ unsign test_tx2)


test_addEntry2 = addEntry test_sig_parse test_tx2 (Entry "" alice (-20) (test_sig test_tx2p)) test_bal


test_addBlock2 = addBlock test_sig_parse test_sig_parse test_block2 test_bal


test_eval2 = eval test_sig_parse test_sig_parse test_parser test_scale test_chain2 test_ic

