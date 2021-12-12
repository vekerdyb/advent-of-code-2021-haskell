import Data.Char
import Debug.Trace

type Edge = (String, String)
type Node = String

splitOn :: Char -> String -> (String, String)
splitOn separator str = (takeWhile (/= separator) str, tail $ dropWhile (/= separator) str)

parse :: String -> IO [(String, String)]
parse  = fmap (map (splitOn '-') . lines) . readFile

undirected :: [Edge] -> [Edge]
undirected edges = concat [[(x,y), (y,x)] | (x, y) <- edges]

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x:unique (filter ((/=) x) xs)

goodNextEdge :: [Node] -> Edge -> [Node] -> Bool
goodNextEdge canVisitTwice (_, to) pathSoFar =
            (all isLower to && to `notElem` pathSoFar) ||
            (
                to `elem` canVisitTwice
                && all (id) [2 > (length $ filter (==cvt) pathSoFar) | cvt <- canVisitTwice]
            ) ||
            (all isUpper to)

paths :: [Node] -> [Edge] -> [Node] -> [[Node]] -> [[Node]]
paths canVisitTwice edges pathSoFar pathsAcc
    | currentNode == "end" = pathSoFar:pathsAcc
    | nextEdges == [] = pathsAcc
    | otherwise = concat [
            paths canVisitTwice edges (pathSoFar ++ [snd nextEdge]) pathsAcc |
            nextEdge <- nextEdges
        ]
    where nextEdges = filter (\edge -> goodNextEdge canVisitTwice edge pathSoFar) allEdgesFromCurrentNode
          currentNode = last pathSoFar
          allEdgesFromCurrentNode = filter (\(from, _) -> from == currentNode) edges

smallNodes :: [Edge] -> [Node]
smallNodes ((from, to):[]) = filter (all isLower) [from, to]
smallNodes (e:edges) = filter (\n -> n `notElem` ["start", "end"]) $ unique $ smallNodes [e] ++ smallNodes edges

main = do
    unidirectional <- parse "day12/input.txt"
    print $ length $ paths (smallNodes unidirectional) (undirected unidirectional) ["start"] []
