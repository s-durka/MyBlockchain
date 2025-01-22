module Blockchain where
import Control.Monad
import Data.Word

import Hashable32
import HashTree
import Utils

type Address = Hash
type Amount = Word32
coin :: Amount
coin = 1000
data Transaction = Tx
  { txFrom :: Address
  , txTo :: Address
  , txAmount :: Amount
  } deriving Show

instance Hashable Transaction where
  hash (Tx a b c) = hash [hash a, hash b, hash c]

data Block = Block { blockHdr :: BlockHeader, blockTxs ::  [Transaction]}

instance Show Block where
  show (Block hdr txs) = unlines (show hdr : map show txs)

instance Hashable Block where
  hash = hash . blockHdr

data BlockHeader = BlockHeader
  {
    parent :: Hash
  , coinbase :: Transaction
  , txroot :: Hash -- root of the Merkle tree
  , nonce :: Hash
  } deriving Show

instance Hashable BlockHeader where
  hash (BlockHeader p c r n) = hash [p,hash c, r, n]

difficulty = 5
blockReward = 50*coin
coinbaseTx miner = Tx {txFrom = 0, txTo = miner, txAmount = blockReward}

validNonce :: BlockHeader -> Bool
validNonce b = (hash b) `mod` (2^difficulty) == 0

tx1 = Tx
  { txFrom = hash "Alice"
  , txTo = hash "Bob"
  , txAmount = 1*coin
  }

type Miner = Address
type Nonce = Word32

mineBlock :: Miner -> Hash -> [Transaction] -> Block
-- mineBlock miner parent txs = mineBlock' 0 miner parent txs
mineBlock = mineBlock' 0 
    where
        mineBlock' :: Hash -> Miner -> Hash -> [Transaction] -> Block
        mineBlock' nonce miner parent txs
            | validNonce (newHdr) == False = tryNext
            | verifyBlock newBlock parent == Nothing = tryNext
            | otherwise = newBlock
            where 
                newHdr = createHeader nonce miner parent txs
                newBlock = Block newHdr txs
                tryNext = mineBlock' (nonce+1) miner parent txs

createHeader :: Hash -> Miner -> Hash -> [Transaction] -> BlockHeader
createHeader nonce miner parent txs 
    = let cb = coinbaseTx miner
    in BlockHeader {parent = parent, coinbase = cb, txroot = (treeHash (buildTree (cb:txs))), nonce = nonce}

genesis = block0
block0 = mineBlock (hash "Satoshi") 0 []
block1 = mineBlock (hash "Alice") (hash genesis) []
block2 = mineBlock (hash "Charlie") (hash block1) [tx1]
chain = [block2, block1, block0]

-- | Chain verification
-- >>> verifyChain [block1, block2]
-- Nothing
--
-- >>> VH <$> verifyChain [block2,block1,block0]
-- Just 0x0dbea380

validChain :: [Block] -> Bool
validChain block = verifyChain block /= Nothing

verifyChain :: [Block] -> Maybe Hash
verifyChain [] = Just 0
verifyChain (b:bs) = mapMaybe (verifyBlock b) (verifyChain bs)
    where
    mapMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
    mapMaybe f Nothing = Nothing
    mapMaybe f (Just x) = f x

verifyBlock :: Block -> Hash -> Maybe Hash
verifyBlock b@(Block hdr txs) parentHash = do
  guard (parent hdr == parentHash)
  guard (txroot hdr == treeHash (buildTree (coinbase hdr:txs))) -- txroot == hash of the merkle tree of [coinbase, transactions]
  guard (validNonce hdr)

  return (hash b)

fromMaybe :: a -> Maybe a -> a
fromMaybe y Nothing = y
fromMaybe _ (Just x) = x


data TransactionReceipt = TxReceipt
  {  txrBlock :: Hash, txrProof :: MerkleProof Transaction } deriving Show

validateReceipt :: TransactionReceipt -> BlockHeader -> Bool
validateReceipt r hdr = txrBlock r == hash hdr
                        && verifyProof (txroot hdr) (txrProof r)

mineTransactions :: Miner -> Hash -> [Transaction] -> (Block, [TransactionReceipt])
mineTransactions miner parent txs = (block, receipts) where 
    fromMaybe' :: Maybe a -> a
    fromMaybe' (Just x) = x
    block = mineBlock miner parent txs
    receipts = fmap createReceipt txs
    txsTree = buildTree $ coinbaseTx miner:txs
    createReceipt :: Transaction -> TransactionReceipt
    createReceipt tx = TxReceipt { txrBlock = (hash block), txrProof = fromMaybe' (buildProof tx txsTree) }

tx = Tx {
    txFrom = 2030195168,
    txTo = 2969638661,
    txAmount = 1000}
proof = MerkleProof (Tx {
    txFrom = 2030195168,
    txTo = 2969638661,
    txAmount = 1000})
