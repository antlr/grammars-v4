module TreeADT (Tree) where 

data Tree a             = Leaf a | Branch (Tree a) (Tree a)
leaf                    = Leaf
branch                  = Branch
cell  (Leaf a)          = a
left  (Branch l r)      = l
right (Branch l r)      = r
isLeaf   (Leaf _)       = True
isLeaf   _              = False