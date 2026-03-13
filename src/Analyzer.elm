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
    , stairsProportion : Float
    , bridgesProportion : Float
    , riverFactor : Float
    , loopCount : Int
    , greenery : Int
    , hanging : Set M.Position
    , holes : Int
    , squares : Int
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

        isHanging block =
            case block of
                M.Stairs pos dir ->
                    let
                        hasDown = M.move pos dir maze /= Nothing
                        hasUp = M.move pos (M.oppositeDirection dir) maze /= Nothing
                    in
                    not (hasDown && hasUp)

                M.Bridge ( x, y, z ) ->
                    let
                        topExits =
                            [ M.SE, M.SW, M.NE, M.NW ]
                                |> List.filter (\dir -> M.move ( x, y, z ) dir maze /= Nothing)

                        bottomExits =
                            [ M.SE, M.SW, M.NE, M.NW ]
                                |> List.filter (\dir -> M.move ( x, y, z - 1 ) dir maze /= Nothing)

                        hasAll list sub =
                            List.all (\item -> List.member item list) sub

                        pair1 = [ M.SE, M.NW ]
                        pair2 = [ M.SW, M.NE ]

                        ok =
                            (hasAll topExits pair1 && hasAll bottomExits pair2)
                                || (hasAll topExits pair2 && hasAll bottomExits pair1)
                    in
                    not ok

                _ -> False

        hanging : Set M.Position
        hanging = blocks |> List.filter isHanging |> List.map M.blockPosition |> Set.fromList

        -- Holes
        minX = maze.offsetX
        minY = maze.offsetY
        maxX = minX + maze.width - 1
        maxY = minY + maze.height - 1

        reachableEmpty =
            let
                isInsideExpanded ( x, y ) =
                    x >= minX - 1 && x <= maxX + 1 && y >= minY - 1 && y <= maxY + 1

                getEmptyNeighbors ( x, y ) =
                    [ ( x + 1, y ), ( x - 1, y ), ( x, y + 1 ), ( x, y - 1 ) ]
                        |> List.filter (\pos -> isInsideExpanded pos && M.get pos maze == Nothing)

                startPoints =
                    List.concat
                        [ List.range (minX - 1) (maxX + 1) |> List.concatMap (\x -> [ ( x, minY - 1 ), ( x, maxY + 1 ) ])
                        , List.range (minY - 1) (maxY + 1) |> List.concatMap (\y -> [ ( minX - 1, y ), ( maxX + 1, y ) ])
                        ]

                loop stack visited =
                    case stack of
                        [] ->
                            visited

                        curr :: rest ->
                            if Set.member curr visited then
                                loop rest visited
                            else
                                loop (getEmptyNeighbors curr ++ rest) (Set.insert curr visited)
            in
            loop startPoints Set.empty

        holes =
            List.range minX maxX
                |> List.concatMap (\x -> List.range minY maxY |> List.map (\y -> ( x, y )))
                |> List.filter (\pos -> M.get pos maze == Nothing && not (Set.member pos reachableEmpty))
                |> List.length

        isBaseAt ( x, y, z ) =
            case M.get ( x, y ) maze of
                Just (M.Base ( _, _, bz )) -> bz == z
                _ -> False

        numSquares =
            blocks
                |> List.filterMap
                    (\b -> case b of
                        M.Base pos -> Just pos
                        _ -> Nothing
                    )
                |> List.filter
                    (\( x, y, z ) ->
                        isBaseAt ( x + 1, y, z ) &&
                        isBaseAt ( x, y + 1, z ) &&
                        isBaseAt ( x + 1, y + 1, z )
                    )
                |> List.length

        numStairs =
            blocks
                |> List.filter
                    (\b -> case b of
                        M.Stairs _ _ -> True
                        _ -> False
                    )
                |> List.length

        numBridges =
            blocks
                |> List.filter
                    (\b -> case b of
                        M.Bridge _ -> True
                        _ -> False
                    )
                |> List.length
    in
    { reachable = isReachable
    , occluding = occluding
    , unreachable = unreachableSet
    , totalCells = v
    , shortestPathLength = shortestPathLength
    , solutionDensity =
        case shortestPathLength of
            Just len -> toFloat len / toFloat v
            Nothing -> 0
    , stairsProportion = toFloat numStairs / toFloat v
    , bridgesProportion = toFloat numBridges / toFloat v
    , riverFactor = riverFactor
    , loopCount = loopCount
    , greenery = greenery
    , hanging = hanging
    , holes = holes
    , squares = numSquares
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
