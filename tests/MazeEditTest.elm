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


toggleStairsTest =
    describe "MazeEdit.toggleStairs"
        [ test "chooses the direction with the most connections" <|
            \_ ->
                let
                    maze = M.fromBlocks [ M.Base ( 0, -1, 0 ), M.Base ( 0, 1, 1 ) ]
                    updatedMaze = ME.toggleStairs ( 0, 0, 1 ) maze
                in
                case M.get ( 0, 0 ) updatedMaze of
                    Just (M.Stairs _ M.SE) -> Expect.pass
                    other -> Expect.fail ("Expected stairs facing SE, got " ++ Debug.toString other)
        , test "uses tie-breaking rule (prefer first in allDirections)" <|
            \_ ->
                let
                    maze = M.fromBlocks [ M.Base ( 0, -1, 0 ) ]
                    updatedMaze = ME.toggleStairs ( 0, 0, 1 ) maze
                in
                case M.get ( 0, 0 ) updatedMaze of
                    Just (M.Stairs _ M.SE) -> Expect.pass
                    other -> Expect.fail ("Expected stairs facing SE (first in allDirections), got " ++ Debug.toString other)
        , test "chooses direction connecting to top only if better than nothing" <|
            \_ ->
                let
                    maze = M.fromBlocks [ M.Base ( 0, -1, 1 ) ]
                    updatedMaze = ME.toggleStairs ( 0, 0, 1 ) maze
                in
                case M.get ( 0, 0 ) updatedMaze of
                    Just (M.Stairs _ M.NW) -> Expect.pass
                    other -> Expect.fail ("Expected stairs facing NW, got " ++ Debug.toString other)
        , test "cycles direction if stairs already exist" <|
            \_ ->
                let
                    maze = M.fromBlocks [ M.Stairs ( 0, 0, 1 ) M.SE ]
                    updatedMaze = ME.toggleStairs ( 0, 0, 1 ) maze
                in
                case M.get ( 0, 0 ) updatedMaze of
                    Just (M.Stairs _ M.SW) -> Expect.pass
                    other -> Expect.fail ("Expected stairs facing SW (next after SE), got " ++ Debug.toString other)
        ]
