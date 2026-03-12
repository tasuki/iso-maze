module Analyzer exposing (Analysis, analyze)

import Dict exposing (Dict)
import Maze as M
import Set exposing (Set)


type alias Analysis =
    { reachable : Bool
    , occluding : Set M.Position
    , unreachable : Set M.Position
    , totalCells : Int
    , shortestPathLength : Maybe Int
    , solutionDensity : Float
    , riverFactor : Float
    , loopCount : Int
    , greenery : Int
    }


getAllCells : List M.Block -> List M.Position
getAllCells =
    List.concatMap (\block ->
        case block of
            M.Base pos -> [ pos ]
            M.Stairs pos _ -> [ pos ]
            M.Bridge ( x, y, z ) -> [ ( x, y, z ), ( x, y, z - 1 ) ]
            M.Greenery pos -> [ pos ]
    )

analyze : M.Maze -> Analysis
analyze maze =
    let
        blocks = M.toBlocks maze
        isGreenery b = case b of
            M.Greenery _ -> True
            _ -> False
        greenery = List.filter isGreenery blocks |> List.length
        allCellsList = getAllCells blocks
        v = List.length allCellsList

        neighborsMap =
            allCellsList
                |> List.map (\pos ->
                    ( pos
                    , [ M.SE, M.SW, M.NE, M.NW ]
                        |> List.filterMap (\dir -> M.move pos dir maze)
                    )
                )
                |> Dict.fromList

        getNeighbors pos = Dict.get pos neighborsMap |> Maybe.withDefault []

        -- BFS for reachability and shortest path
        start = M.startPosition maze
        end = M.endPosition maze
        bfsDistances = bfs start getNeighbors
        reachableSet = Dict.keys bfsDistances |> Set.fromList
        isReachable = Set.member end reachableSet
        unreachableSet = Set.diff (Set.fromList allCellsList) reachableSet
        shortestPathLength = Dict.get end bfsDistances

        -- Components
        c = findComponents allCellsList Set.empty 0 getNeighbors

        -- Edges (undirected)
        e =
            allCellsList
                |> List.concatMap
                    (\pos ->
                        getNeighbors pos
                            |> List.map (\n -> if compare pos n == LT then ( pos, n ) else ( n, pos ))
                    )
                |> Set.fromList
                |> Set.size

        -- Junctions and Dead ends
        junctions =
            allCellsList |> List.filter (\pos -> List.length (getNeighbors pos) >= 3) |> List.length

        deadEnds =
            allCellsList |> List.filter (\pos -> List.length (getNeighbors pos) == 1) |> List.length

        riverFactor =
            if (junctions + deadEnds) == 0 then 0
            else toFloat v / toFloat (junctions + deadEnds)

        loopCount = e - v + c

        isOccluded k h x y =
            if h - k <= 0 then False
            else
                case M.get ( x + k, y + k ) maze of
                    Nothing -> isOccluded (k + 1) h x y
                    Just targetBlock ->
                        if M.positionZ (M.blockPosition targetBlock) < h - k then True
                        else isOccluded (k + 1) h x y

        isOccluding block =
            let
                ( x, y, _ ) = M.blockPosition block
                h = M.positionZ (M.blockPosition block)
                adjustedH =
                    case block of
                        M.Stairs _ M.NW -> h - 1
                        M.Stairs _ M.NE -> h - 1
                        _ -> h
            in
            isOccluded 1 adjustedH x y

        occluding : Set M.Position
        occluding = blocks |> List.filter isOccluding |> List.map M.blockPosition |> Set.fromList
    in
    { reachable = isReachable
    , occluding = occluding
    , unreachable = unreachableSet
    , totalCells = v
    , shortestPathLength = shortestPathLength
    , solutionDensity =
        case shortestPathLength of
            Just len ->
                if v == 0 then 0
                else toFloat len / toFloat v
            Nothing ->
                0
    , riverFactor = riverFactor
    , loopCount = loopCount
    , greenery = greenery
    }


bfs : M.Position -> (M.Position -> List M.Position) -> Dict M.Position Int
bfs start getNeighbors =
    let
        loop currentLevel nextLevel dist visited distances =
            case currentLevel of
                [] ->
                    if nextLevel == [] then distances
                    else loop nextLevel [] (dist + 1) visited distances
                current :: rest ->
                    if Set.member current visited then
                        loop rest nextLevel dist visited distances
                    else
                        let
                            neighbors = getNeighbors current
                                |> List.filter (\n -> not (Set.member n visited))
                            newDistances =
                                List.foldl (\n acc -> Dict.insert n (dist + 1) acc) distances neighbors
                        in
                        loop rest (neighbors ++ nextLevel) dist (Set.insert current visited) newDistances
    in
    loop [ start ] [] 0 Set.empty (Dict.singleton start 0)


findComponents : List M.Position -> Set M.Position -> Int -> (M.Position -> List M.Position) -> Int
findComponents remaining visited count getNeighbors =
    case remaining of
        [] -> count
        pos :: rest ->
            if Set.member pos visited then
                findComponents rest visited count getNeighbors
            else
                let
                    component =
                        floodFill [ pos ] Set.empty getNeighbors
                in
                findComponents rest (Set.union visited component) (count + 1) getNeighbors


floodFill : List M.Position -> Set M.Position -> (M.Position -> List M.Position) -> Set M.Position
floodFill stack visited getNeighbors =
    case stack of
        [] -> visited
        current :: rest ->
            if Set.member current visited then
                floodFill rest visited getNeighbors
            else
                floodFill (getNeighbors current ++ rest) (Set.insert current visited) getNeighbors
