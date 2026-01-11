module Decorations exposing (..)

import Maze as M

-- The general idea is that the maze can be shown with various decorations
-- which depend on the overall shape of the maze. Here goes the logic...

getRailings : M.Maze -> List ( M.Block, M.Direction )
getRailings maze =
    let
        isAbove : (M.Block, M.Direction) -> Bool
        isAbove ( block, dir ) =
            let ( x, y, z ) = M.blockPosition block in
            case M.move ( x, y, z ) dir maze of
                Just _ -> False -- can move
                Nothing -> -- can't move
                    case M.get (M.shiftPos2d ( x, y ) dir) maze of
                        Just (M.Base ( _, _, nz )) -> z > nz
                        Just (M.Bridge ( _, _, nz )) -> z > nz
                        -- supposing no stairs from bridge! don't do that!
                        Just (M.Stairs ( _, _, nz ) _) -> z + 1 > nz
                        Nothing -> True

        determineRailings : M.Block -> List ( M.Block, M.Direction )
        determineRailings block =
            List.map (Tuple.pair block) M.allDirections
                |> List.filter isAbove
    in
    M.toBlocks maze |> List.concatMap determineRailings
