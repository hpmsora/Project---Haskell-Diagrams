{-# LANGUAGE NoMonomorphismRestriction,
             FlexibleContexts, TypeFamilies #-}
{-# OPTIONS -W #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

main :: IO ()
main = mainWith (frame 0.1 diagram1)

--atPoints (trailVertices $ regPoly 8 1) (repeat node)

diagram :: Diagram SVG
diagram = atPoints (trailVertices $ regPoly 6 1) (repeat diagram1)

diagram1 :: Diagram SVG
diagram1 = atPoints (trailVertices $ regPoly 15 1) (repeat diagram2)

diagram2 :: Diagram SVG
diagram2 = atPoints (trailVertices $ regPoly 15 1) (repeat diagram3)

diagram3 :: Diagram SVG
diagram3 = atPoints (trailVertices $ regPoly 15 1) (repeat (strokeLine (f 40)))

f :: Int -> Trail' Line V2 Double
f 0 = mempty
f n = onLineSegments tail (circle 0.1)
  `mappend` rotateBy 0.025 (f (n-1))

everyOther :: [a] -> [a]
everyOther (x:xs) = x : everyOther' xs
everyOther []     = []

everyOther' :: [a] -> [a]
everyOther' (_:x:xs) = x : everyOther' xs
everyOther' _        = []

