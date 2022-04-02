module HashTree where
import Hashable32

data Tree a = Leaf Hash a | Node Hash (Tree a) (Tree a) | Twig Hash (Tree a)
    deriving Show
instance Eq a => Eq (Tree a) where
    Leaf h x == Leaf h' y = x == y
    Node h l r == Node h' l' r' = (h==h')&&(l==l')&&(r==r')

drawTree :: Show a => Tree a -> String
drawTree t
    = let
        showTabs :: Int -> ShowS
        showTabs 0 = showString ""
        showTabs n = showString "  " . showTabs (n-1)
        dt :: Show a => Int -> Tree a -> ShowS -- int value denotes number of tabs
        dt i (Leaf h x) = showTabs i . showString (showHash h) . shows x . showString "\n"
        dt i (Node h l r) = showTabs i . showString (showHash h) . showString "-\n" . dt (i+1) l . dt (i+1) r
        dt i (Twig h l) = showTabs i . showString (showHash h) . showString "+\n" . dt (i+1) l
    in dt 0 t ""


leaf :: Hashable a => a -> Tree a
leaf x = Leaf (hash x) x

twig :: Hashable a => Tree a -> Tree a
twig t = let h = treeHash t in Twig (hash (h,h)) t

node :: Hashable a =>  Tree a -> Tree a -> Tree a
node l r = Node (hash (treeHash l, treeHash r)) l r

buildTree :: Hashable a => [a] -> Tree a
buildTree list = 
    let 
        buildLevel :: Hashable a => [Tree a] -> [Tree a]
        buildLevel [] = []
        buildLevel (l:r:xs) = (node l r):(buildLevel xs)
        buildLevel [l] =  [twig l]
    in let
        buildTree' :: Hashable a => [Tree a] -> [Tree a]
        buildTree' [root] = [root]
        buildTree' l = buildTree' $ buildLevel l    -- pattern match for lists of length > 1
    in let leaves = fmap leaf list 
    in let [t] = buildTree' leaves
    in t

treeHash :: Tree a -> Hash
treeHash (Leaf h _) = h
treeHash (Node h _ _) = h
treeHash (Twig h _) = h

-- the information of where our node lies is in the *constructor* (Left/Right),
-- and the Hash value holds the hash of the *other* child tree
type MerklePath = [Either Hash Hash]

showMerklePath' :: MerklePath -> ShowS
-- showMerklePath path = fmap (VH . fromEither) path
showMerklePath' ((Right h):hs) = showString ">" . showString (showHash h) . showMerklePath' hs
showMerklePath' ((Left h):hs) = showString "<" . showString (showHash h) . showMerklePath' hs
showMerklePath' [] = showString ""

showMerklePath :: MerklePath -> String
showMerklePath path = showMerklePath' path ""

-- data MerkleProof a = MerkleProof a MerklePath
--     deriving Show

data MerkleProof a = MerkleProof a MerklePath

instance Show a => Show (MerkleProof a) where
    show (MerkleProof x merklePath) = g x merklePath "" where
        g :: Show a => a -> MerklePath -> ShowS
        g x path = showString "MerkleProof " . shows x . showMerklePath' path
    -- show = g "" where
    --     g :: (MerkleProof a) -> ShowS
    --     g (MerkleProof x path) = showString "MerkleProof " . shows x . showMerklePath' path


buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof x t
    | paths == [] = Nothing
    | otherwise = Just $ MerkleProof x (head paths)
    where paths = merklePaths x t

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths x (Leaf h val) = if hash x == h then [[]] else []
merklePaths x (Node h l r)
    = let
        leftMPs = merklePaths x l                           -- lazy eval
        rightMPs = merklePaths x r                          -- lazy eval
        leftHash = Right $ treeHash l :: Either Hash Hash   -- hash of left subtree, path goes right
        rightHash = Left $ treeHash r :: Either Hash Hash   -- hash of right subtree, path goes down
    in let
        -- append each Merkle Path in the subtree with the hash value of the opposite branch
        lmps = if leftMPs == [] then [] else fmap (rightHash:) leftMPs  -- x is in the left subtree
        rmps = if rightMPs == [] then [] else fmap (leftHash:) rightMPs  -- x is in the right subtree
    in
        lmps ++ rmps
merklePaths x (Twig h l)
    = let
        leftMPs = merklePaths x l
        -- no right subtree => copy hash of left subtree (treeHash r := treeHash l)
        rightHash = Left $ treeHash l :: Either Hash Hash
    in  -- append each Merkle Path in the subtree with the hash value of the opposite branch
        if leftMPs == [] then [] else fmap (rightHash:) leftMPs 

