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
                               key = head args
                               value = [(args!!1, stringToFloat (args !! 2))] in
        buildGraph xs (Data.Map.insertWith (++) key value graph)

stringToFloat :: String -> Float
stringToFloat s = read s :: Float