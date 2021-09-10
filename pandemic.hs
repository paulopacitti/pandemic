module Main where
import Data.Map ( empty, insertWith, Map )
import Control.Monad ()

type Graph = Map String [(String, Float)]

main = do
    contents <- getContents
    let graph = buildGraph (init (lines contents)) Data.Map.empty
    let pacient0 = last (lines contents)
    print graph -- print the graph
    putStrLn ""


buildGraph :: [String] -> Graph -> Graph
buildGraph [] graph  = graph
buildGraph (x:xs) graph  = let args = words x
                               v = head args
                               u = args!!1
                               weight = stringToFloat (args !! 2) in
        buildGraph xs (Data.Map.insertWith (++) v [(u, weight)] (Data.Map.insertWith (++) u [(v, weight)] graph)) -- insert in both adjacency lists

stringToFloat :: String -> Float
stringToFloat s = read s :: Float