import Data.Char

type Edge = (String, String)
type Node = String

splitOn :: Char -> String -> (String, String)
splitOn separator str = (takeWhile (/= separator) str, tail $ dropWhile (/= separator) str)

parse :: String -> IO [(String, String)]
parse  = fmap (map (splitOn '-') . lines) . readFile

undirected :: [Edge] -> [Edge]
undirected edges = concat [[(x,y), (y,x)] | (x, y) <- edges]

goodNextEdge :: Edge -> [Node] -> Bool
goodNextEdge (_, to) pathSoFar = (all isLower to && to `notElem` pathSoFar) || (all isUpper to)

getPath :: [Edge] -> [Node] -> [[Node]] -> [[Node]]
getPath edges pathSoFar pathsAcc
    | currentNode == "end" = pathSoFar:pathsAcc
    | nextEdges == [] = pathsAcc
    | otherwise = concat [getPath edges (pathSoFar ++ [snd nextEdge]) pathsAcc | nextEdge <- nextEdges]
    where nextEdges = filter (\edge -> goodNextEdge edge pathSoFar) allEdgesFromCurrentNode
          currentNode = last pathSoFar
          allEdgesFromCurrentNode = filter (\(from, _) -> from == currentNode) edges

main = do
    unidirectional <- parse "day12/input.txt"
    let edges = undirected unidirectional
    print $ length $ getPath edges ["start"] []
