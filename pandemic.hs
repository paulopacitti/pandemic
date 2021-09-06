module Main where
import Data.Map

type Graph = Map String [(String, Float)]

main = do   
    userInput <- getContents
    mapM_ putStr userInput


-- buildGraph :: (Graph, String) -> (Graph, String)
-- process graph string = do
--     line <- getLine
--     let args = words line in
--         if length args == 3
--             then (process (Data.Map.insertWith (\new old -> old ++ new) (head args) [(args!!1, args!!2)] (fst t)),"")
--             else (graph,head args)


-- buildGraph :: Graph -> [String] -> (Graph, String)
-- buildGraph graph args = if length args == 1 then
--         (graph, head args)
--     else do line <- getLine
--         let newGraph = Data.Map.insertWith (\new old -> old ++ new) (head args) [(args!!1, args!!2)] graph
--         return buildGraph graph (words line)

-- buildTuple graph start = (graph, start)