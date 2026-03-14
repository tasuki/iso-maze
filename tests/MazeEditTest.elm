module MazeEditTest exposing (..)

import Expect
import Maze as M
import MazeEdit as ME
import Test exposing (..)

clearTileTest =
    describe "MazeEdit.clearTile"
        [ test "clears a block at the given position" <|
            \_ ->
                let
                    maze = M.fromBlocks [ M.Base ( 0, 0, 0 ) ]
                    updatedMaze = ME.clearTile ( 0, 0, 0 ) maze
                in
                Expect.equal Nothing (M.get ( 0, 0 ) updatedMaze)
        , test "does nothing if no block is at the position" <|
            \_ ->
                let
                    maze = M.fromBlocks [ M.Base ( 1, 1, 0 ) ]
                    updatedMaze = ME.clearTile ( 0, 0, 0 ) maze
                in
                Expect.equal (M.get ( 1, 1 ) maze) (M.get ( 1, 1 ) updatedMaze)
        ]
