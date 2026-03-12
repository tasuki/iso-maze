module Analyzer exposing (Analysis, analyze)

import Dict exposing (Dict)
import Maze as M
import Set exposing (Set)


type alias Analysis =
    { reachable : Bool
    , totalCells : Int
    , greenery : Int
    , unreachable : Int
    , shortestPathLength : Maybe Int
    , solutionDensity : Float
    , riverFactor : Float
    , loopCount : Int
    , occluding : Set M.Pos2d
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

        limits = M.getLimits maze
        allPos = M.mapCoords (List.range limits.minY limits.maxY) (List.range limits.minX limits.maxX) (\y x -> ( x, y ))

        isOccluding ( x, y ) =
            case M.get ( x, y ) maze of
                Nothing -> False
                Just block ->
                    let
                        h = M.positionZ (M.blockPosition block)
                        check k =
                            let
                                targetPos = ( x + k, y + k )
                            in
                            if x + k > limits.maxX || y + k > limits.maxY then False
                            else
                                case M.get targetPos maze of
                                    Nothing -> check (k + 1)
                                    Just targetBlock ->
                                        let
                                            hk = M.positionZ (M.blockPosition targetBlock)
                                        in
                                        if hk < h - k then True
                                        else if h - k <= 0 then False
                                        else check (k + 1)
                    in
                    check 1

        occluding = allPos |> List.filter isOccluding |> Set.fromList
    in
    { reachable = isReachable
    , totalCells = v
    , greenery = greenery
    , unreachable = v - Set.size reachableSet - greenery
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
    , occluding = occluding
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
