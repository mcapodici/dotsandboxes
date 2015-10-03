module Data.DotsAndBoxes.Test.StringsAndCoins where

andBack :: [(Int, Int)] -> [(Int, Int)] 
andBack xs = xs ++ [(j, i) | (i, j) <- xs, j /= 0]

c1 = mkUGraph [0..1] $ andBack [(1, 0)] :: Gr () ()
c2 = mkUGraph [0..3] $ andBack [(1, 2), (2, 0), (2, 3)] :: Gr () ()
c3 = mkUGraph [0..2] $ andBack [(1, 2)] :: Gr () ()
c4 = mkUGraph [0..3] $ andBack [(1, 2), (2, 3)] :: Gr () ()
l1 = mkUGraph [0..2] $ andBack [(1, 2), (2, 0)] :: Gr () ()
l2 = mkUGraph [0..3] $ andBack [(1, 2), (2, 3), (3, 0)] :: Gr () ()
lp = mkUGraph [0..4] $ andBack [(1, 2), (1, 3), (2, 4), (3, 4)] :: Gr () () 
ns3 = mkUGraph [0..4] $ andBack [(1, 2), (1, 3), (2, 4), (3, 4), (1, 0), (1, 0), (2, 0)] :: Gr () ()
b = [c1, c2, c3, c4, l1, l2, lp]


