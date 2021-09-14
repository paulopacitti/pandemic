module Main where

import Data.List
import Text.Printf

data Edge = Edge { node::Node, weight::Float } deriving (Show)
type Node = String
type Graph = [(Node, [Edge])]
type Dnode = (Node, (Float, Node))

-- Helper methods
stringToFloat :: String -> Float
stringToFloat s = read s :: Float

readInputData :: [String] -> ((String, String), Float)
readInputData [] = error "incorrect input"
readInputData [u, v, w] = ((u, v), stringToFloat w)

buildGraph :: String -> Graph
buildGraph input =
  let element = map (readInputData . words) $ init (lines input)
      graphElements = appendReversedEdges element
  in fromList graphElements

appendReversedEdges :: [((String, String), Float)] -> [((String, String), Float)]
appendReversedEdges es = es ++ map (\((u,v),w) -> ((v,u),w)) es

-- Takes a list of pairs where the first element is a two-member list 
-- of nodes in any order and the second element is the weight for the edge connecting them.
fromList :: [((String, String), Float)] -> Graph
fromList es =
  let nodes = nub . map (fst . fst) $ es
      edgesFor es node =
        let connected = filter (\((n,_),_) -> node == n) $ es
        in map (\((_,n),wt) -> Edge n wt) connected
  in map (\n -> (n, edgesFor es n)) nodes

-- Given a weighted graph and a node, return the edges incident on the node
edgesFor :: Graph -> Node -> [Edge]
edgesFor g n = snd . head . filter (\(nd, _) -> nd == n) $ g

-- Given a node and a list of edges, one of which is incident on the node, return the weight
weightFor :: Node -> [Edge] -> Float
weightFor n = weight . head . filter (\e -> n == node e)

-- Given a list of edges, return their nodes
connectedNodes :: [Edge] -> [Node]
connectedNodes = map node

-- Core algorithms

-- Given a graph and a start node
dijkstra :: Graph -> Node -> [Dnode]
dijkstra g start =
  let dnodes = initD g start
      unchecked = map fst dnodes
  in  dijkstra' g dnodes unchecked

-- Given a graph and a start node, construct an initial list of Dnodes
initD :: Graph -> Node -> [Dnode]
initD g start =
  let initDist (n, es) =
        if n == start
        then 0
        else if start `elem` connectedNodes es
             then weightFor start es
             else 1.0/0.0
  in map (\pr@(n, _) -> (n, ((initDist pr), start))) g

-- Dijkstra's algorithm (recursive)
dijkstra' :: Graph -> [Dnode] -> [Node] -> [Dnode]
dijkstra' g dnodes [] = dnodes
dijkstra' g dnodes unchecked =
  let dunchecked = filter (\dn -> (fst dn) `elem` unchecked) dnodes
      current = head . sortBy (\(_,(d1,_)) (_,(d2,_)) -> compare d1 d2) $ dunchecked
      c = fst current
      unchecked' = delete c unchecked
      edges = edgesFor g c
      cnodes = intersect (connectedNodes edges) unchecked'
      dnodes' = map (\dn -> update dn current cnodes edges) dnodes
  in dijkstra' g dnodes' unchecked'

-- given a Dnode to update, the current Dnode, the Nodes connected to current 
-- and current's edges, return a (possibly) updated Dnode
update :: Dnode -> Dnode -> [Node] -> [Edge] -> Dnode
update dn@(n, (nd, p)) (c, (cd, _)) cnodes edges =
  let wt = weightFor n edges
  in  if n `notElem` cnodes then dn
      else if cd+wt < nd then (n, (cd+wt, c)) else dn

main = do
    contents <- getContents
    let graph = buildGraph contents
    let pacient0 = last (lines contents)
    putStrLn ""
