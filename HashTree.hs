module HashTree where
import Hashable32

data Tree a = Leaf Hash a | Node Hash (Tree a) (Tree a) | Twig Hash (Tree a)
    deriving Show
instance Eq a => Eq (Tree a) where
    Leaf h x == Leaf h' y = x == y
    Node h l r == Node h' l' r' = (h==h')&&(l==l')&&(r==r')

leaf :: Hashable a => a -> Tree a
leaf x = Leaf (hash x) x

twig :: Hashable a => Tree a -> Tree a
twig t = let h = treeHash t in Twig (hash (h,h)) t

node :: Hashable a =>  Tree a -> Tree a -> Tree a
node l r = Node (hash (treeHash l, treeHash r)) l r

buildTree :: Hashable a => [a] -> Tree a
buildTree list = 
    let 
        leafList :: Hashable a => [a] -> [Tree a]
        leafList [] = []
        leafList (x:xs) = (leaf x):(leafList xs)
        buildTree' :: Hashable a => Int -> [Tree a] -> [Tree a]
        -- the integer value denotes whether or not this is the first index of the list
        -- (0 <=> yes, 1 <=> no)
        buildTree' _ [] = []
        buildTree' _ (l:r:xs) = buildTree' 0 ((node l r):(buildTree' 1 xs))
        buildTree' 1 (l:_) =  [twig l] -- due to pattern matching, we know this is a (sub)list of size 1 
        buildTree' 0 (l:_) = [l]       -- the whole list consists of a single element
    in let leaves = leafList list 
    in let [t] = buildTree' 0 leaves
    in t

treeHash :: Tree a -> Hash
treeHash (Leaf h _) = h
treeHash (Node h _ _) = h
treeHash (Twig h _) = h

-- drawTree :: Show a => Tree a -> String

-- the information of where our node lies is in the *constructor* (Left/Right),
-- and the Hash value holds the hash of the *other* child tree
type MerklePath = [Either Hash Hash] 


data MerkleProof a = MerkleProof a MerklePath

-- buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths x (Leaf h val) = []
merklePaths x (Node h l r)
    = let
        leftMPs = merklePaths x l                           -- lazy eval
        rightMPs = merklePaths x r                          -- lazy eval
        leftHash = Right $ treeHash l :: Either Hash Hash   -- hash of left subtree, path goes right
        rightHash = Left $ treeHash r :: Either Hash Hash   -- hash of right subtree, path goes down
    in let
        -- append each Merkle Path in the subtree with the hash value of the opposite branch
        lmps = if leftMPs == [] && not (treeHash l == hash x) 
            then []                         -- x is not in the left subtree
            else fmap (rightHash:) leftMPs  -- x is in the left subtree
        rmps = if rightMPs == [] && not (treeHash r == hash x) 
            then []                         -- x is not in the right subtree
            else fmap (leftHash:) rightMPs  -- x is in the right subtree
    in
        lmps ++ rmps
merklePaths x (Twig h l)
    = let
        leftMPs = merklePaths x l                   -- lazy eval
        -- no right subtree => copy hash of left subtree
        rightHash = Left $ treeHash l :: Either Hash Hash   
    in
        -- append each Merkle Path in the subtree with the hash value of the opposite branch
        if leftMPs == [] && not (treeHash l == hash x) then [] -- x is not in the subtree
        else fmap (rightHash:) leftMPs                         -- x is in the subtree
