module Decorations exposing (..)

import Maze as M
import Maybe.Extra

-- The general idea is that the maze can be shown with various decorations
-- which depend on the overall shape of the maze. Here goes the logic...

getRailings : M.Maze -> List ( M.Block, M.Direction )
getRailings maze =
    let
        canMoveStairs : M.Position -> M.Direction -> Bool
        canMoveStairs pos dir =
            M.canExit pos dir maze
                |> Maybe.Extra.isJust

        isAbove : Int -> M.Direction -> Maybe M.Block -> Bool
        isAbove z dir maybeNeighbor =
            case maybeNeighbor of
                Nothing -> True
                Just (M.Base ( _, _, nz )) -> z > nz
                Just (M.Bridge ( _, _, nz )) -> z > nz
                Just (M.Stairs ( nx, ny, nz ) _) ->
                    if z > nz then
                        True
                    else if z == nz then
                        not <| canMoveStairs ( nx, ny, nz ) dir
                    else
                        False

        hasRailing : ( M.Block, M.Direction ) -> Bool
        hasRailing ( block, dir ) =
            case block of
                M.Base ( x, y, z ) ->
                    M.get (M.shiftPos2d ( x, y ) dir) maze
                        |> isAbove z dir
                _ -> False

        determineRailings : M.Block -> List ( M.Block, M.Direction )
        determineRailings block =
            List.map (Tuple.pair block) M.allDirections
                |> List.filter hasRailing
    in
    M.toBlocks maze |> List.concatMap determineRailings
