import Distribution.Simple.Test.Log (countTestResults)

type Point = (Double, Double)

last1:: (Point, Point, Double) -> Double
last1 (x,y,z ) = z

minList:: [(Point, Point, Double)] ->(Point, Point, Double)
minList (s:list) = foldl (\acc y -> if (last1 y)< (last1 acc) then y else acc) s list

dist :: Point -> Point-> (Point,Point, Double) 
dist (a, b) (c, d) = ((a,b),(c, d),sqrt ((a-c)*(a-c)+(d-b)*(d-b)))

minDistP :: Point ->[Point]->(Point, Point, Double)
minDistP p list = minList (map (\x -> (dist x p)) (filter (/=p) (list)))

minDist :: [Point] -> (Point, Point, Double)
minDist list = minList (map (\x-> (minDistP x list)) list)
