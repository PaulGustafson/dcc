module Test where
  import Chain
  import Data.Char
  import qualified Data.Map as M
  import Data.Serialize
  import Crypto.Hash

  test_owf1 :: SBlock -> Hash
  test_owf1 = show . hashWith SHA256 . encode . show
  test_owf1_src = "show . hashWith SHA256 . encode . show"

  test_owf2 :: SBlock -> Hash
  test_owf2 = show . hashWith SHA512 . encode . show
  test_owf2_src = "show . hashWith SHA512 . encode . show"

  allowed_owfs = [test_owf1_src, test_owf2_src]

  test_blkRwd = const 1
  test_blkRwdSrc = "const 1"

  test_scale :: Scale
  test_scale sb =
    if length (show sb) < 999
       && owfSrc (ublock sb) `elem` allowed_owfs
    then Just $ (10 - blkRwd (ublock sb) sb)
           * (fromIntegral $ length $ filter (== '0') $ (owf $ ublock sb) sb)
    else Nothing

  test_bal :: Balance
  test_bal = M.fromList $ zip ["alice", "bob", "fred", "edna"] $ repeat 100

  test_tx = Tx {
    header = "Hello, world"
    , senders = [("alice", 20)]
    , receivers = [("louis", 10)]
  }

  test_ub = UBlock {
     previous = ""
     , txs = [test_tx]
     , owf = test_owf1
     , owfSrc = test_owf1_src
     , blkRwd = test_blkRwd
     , rwdSrc = test_blkRwdSrc
  }

  test_sb = SBlock {
    ublock = test_ub
    , miner = "fred"
    , nonce = "test_nonce"
  }


  test_chain :: Chain
  test_chain = [test_sb]

  test_addBlock = addBlock test_sb test_bal

  -- test initial conditions
  test_ic = ("", 0, test_bal)

  test_eval = eval test_scale test_chain test_ic


  test_ub2 = UBlock {
     previous = "075a1c1cbdf0a12d1ae6c7e2b1f9a70e80e4644c808cae62efbf41ebb753db30"
     , txs = [test_tx]
     , owf = test_owf2
     , owfSrc = test_owf2_src
     , blkRwd = test_blkRwd
     , rwdSrc = test_blkRwdSrc
  }

  test_sb2 = SBlock {
    ublock = test_ub2
    , miner = "fred"
    , nonce = "test_nonce"
  }

  test_chain2 :: Chain
  test_chain2 = [test_sb2, test_sb]

  test_eval2 = eval test_scale test_chain2 test_ic
