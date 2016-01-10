module Questions54Ato60
{-( Tree(Empty, Branch)-}
{-, leaf-}
{-, btreeNodes-}
{-, btreeHeight-}
{-, btreeHeightBalanced-}
{-, cbalTree-}
{-, symmetric-}
{-, construct-}
{-, symCbalTrees-}
{-, hbalTree-}
{-)-}
where

import Math(fibs)

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

cbalTreeWithElement :: a -> Int -> [Tree a]
cbalTreeWithElement _ 0 = [Empty]
cbalTreeWithElement e 1 = [leaf e]
cbalTreeWithElement e n =
    if r `mod` 2 == 0 then
        [Branch e left right | left <- cbalTreeWithElement e q, right <- cbalTreeWithElement e q]
    else
        concat [[Branch e s1 s2, Branch e s2 s1] | s1 <- cbalTreeWithElement e q,
                                                   s2 <- cbalTreeWithElement e (q+r)]
    where (q, r) = (n-1) `quotRem` 2

btreeNodes :: Tree a -> Int
btreeNodes Empty = 0
btreeNodes (Branch _ left right) =
    1 + btreeNodes left + btreeNodes right

btreeHeight :: Tree a -> Int
btreeHeight Empty = 0
btreeHeight (Branch _ left right) =
    1 + max (btreeHeight left) (btreeHeight right)

btreeHeightBalanced :: Tree a -> Bool
btreeHeightBalanced Empty = True
btreeHeightBalanced (Branch _ left right) =
    (abs (btreeHeight left - btreeHeight right) <= 1) &&
    btreeHeightBalanced left &&
    btreeHeightBalanced right

-- Problem 55
cbalTree :: Int -> [Tree Char]
cbalTree = cbalTreeWithElement 'x'

-- Problem 56
symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ left right) = mirror left right

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ left1 right1) (Branch _ left2 right2) =
    mirror left1 right2 && mirror right1 left2
mirror _ _ = False

-- Problem 57
-- XXX: How to property test (symmetric . construct)?
add :: Ord a => a -> Tree a -> Tree a
add x Empty = Branch x Empty Empty
add x t@(Branch y l r) =
    case compare x y of
        LT -> Branch y (add x l) r
        GT -> Branch y l (add x r)
        EQ -> t

construct :: [Int] -> Tree Int
construct = foldl (flip add) Empty

-- Problem 58
symCbalTrees :: Int -> [Tree Char]
symCbalTrees n
    | even n = []
    | otherwise = filter symmetric $ cbalTree n

-- Problem 59
hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree x 1 = [Branch x Empty Empty]
hbalTree x h = [Branch x l r |
        (hl, hr) <- [(h-2, h-1), (h-1, h-1), (h-1, h-2)],
        l <- hbalTree x hl, r <- hbalTree x hr]

-- Problem 60
-- http://user.it.uu.se/~justin/Teaching/PK2/Slides/AVLtrees.pdf
minNodes :: Int -> Int
minNodes height = fibs !! (height+2) - 1

maxNodes :: Int -> Int
maxNodes height = 2^height - 1

minHeight :: Int -> Int
minHeight nodes =
    ceiling $ logBase (2 :: Float) $ fromIntegral (nodes+1)

maxHeight :: Int -> Int
maxHeight nodes =
    ceiling $ 1.44 * logBase (2 :: Float) (fromIntegral (nodes+1)) - 1.33

hbalTreeNodes :: a -> [Tree a]
hbalTreeNodes = undefined
