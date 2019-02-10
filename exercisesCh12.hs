data Tree a = Leaf | Node (Tree a) a (Tree a) deriving(Show)

instance Functor Tree where
    fmap f Leaf = Leaf
    fmap f (Node c1 v c2) = Node (fmap f c1) (f v) (fmap f c2)

newtype Ziplist a = Z[a] deriving Show

instance Functor Ziplist where
    fmap g (Z (x:xs)) = Z (map g (x:xs))

instance Applicative Ziplist where
    pure x = Z(x:(pure x))
    (<*>) (Z gs) (Z xs) = Z [ g x | (g,x) <- zip gs xs]

main = do
    putStrLn $ "Placeholder"

-- instance Functor ((->) a) where
--     fmap :: (a -> b) -> f a -> f b
--     fmap f g = (\x -> f (g x))

-- instance Applicative ((->) a) where
--     pure g = id g 
--     -- f :: (a)
--     (<*>) f g = (\x -> ((f x) (g x)))




    