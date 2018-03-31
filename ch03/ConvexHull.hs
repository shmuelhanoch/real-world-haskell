import Data.List (sortBy)

type Point = (Int, Int)

data Orientation = Clockwise | Counterclockwise | Colinear
                 deriving (Eq, Show)

orientation :: Point -> Point -> Point -> Orientation
orientation (x1, y1) (x2, y2) (x3, y3)
  | cross > 0  = Clockwise
  | cross < 0  = Counterclockwise
  | cross == 0 = Colinear
  where
    cross = (y2 - y1) * (x3 - x2) - (y3 - y2) * (x2 - x1) 

orientCompare :: Point -> Point -> Point -> Ordering
orientCompare p x y =
  case orientation p x y of
    Clockwise        -> GT
    Counterclockwise -> LT
    Colinear         -> EQ

sortByOrientation :: [Point] -> [Point]
sortByOrientation (p:ps) = p : sortBy (orientCompare p) ps
sortByOrientation [] = error "empty list"

extractMin :: (Ord a) => [a] -> [a]
extractMin [] = error "empty list"
extractMin [x] = [x]
extractMin (x:xs) =
  let y:ys = extractMin xs
  in if x < y then x:y:ys else y:x:ys

convexHull :: [Point] -> [Point]
convexHull ps =
  let
    p0:p1:p2:rest = sortByOrientation $ extractMin ps
  in
    convexHull' rest [p2, p1, p0]

convexHull' :: [Point] -> [Point] -> [Point]
convexHull' [] res = res
convexHull' (p:ps) (q0:q1:qs) =
  case orientation q1 q0 p of
  Clockwise        -> convexHull' ps (p:q1:qs)
  Counterclockwise -> convexHull' ps (p:q0:q1:qs)
  Colinear         -> convexHull' ps (q0:q1:qs)
convexHull' _ _ = error "need at least two points in stack"

main :: IO ()
main = do
  content <- getContents
  let 
    points = (map (((\[x, y] -> (x, y)). map read). words). lines) content
    ans =  convexHull points
  print ans
