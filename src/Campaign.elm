module Campaign exposing (..)

import Codec
import Maze as M
import Set exposing (Set)

type alias Level =
    { name: String
    , emoji: String
    , maze: M.Maze
    , finished: Bool
    }

type alias MazeDef =
    { name: String
    , emoji: String
    , maze: String
    }

mazeDefs : List MazeDef
mazeDefs =
    [ { name = "butterfly"
      , emoji = "🦋"
      , maze = "sz:9,9;off:-3,-3;st:0,0;end:4,4;mz:"
        ++ "x x o2o2o3o3o3o3o3"
        ++ "x x o2z3o3z4z5o5o3"
        ++ "x o1s2o1o3o3o3s5o3"
        ++ "o0o1o1o1z2o2o3s4o3"
        ++ "o0o0s1o1o1s2o3o3o3"
        ++ "x o0o0o0o1o1o1s3o2"
        ++ "x x x o0z1o1z2o2o2"
        ++ "x x x o0o0o1o1x x "
        ++ "x x x x o0o0x x x "
      }
    , { name = "snail"
      , emoji = "🐌"
      , maze = "bg:770;right:366,20;"
        ++ "sz:8,9;off:-3,-2;st:4,6;end:-2,-1;mz:"
        ++ "x x o2z3z4o4z5o5"
        ++ "x o0o2o2o3z4o4s5"
        ++ "o0o0S3o1o3o3o4o4"
        ++ "S1o0o3o2o2o3l4o3"
        ++ "o1z2l3o2o2o3o4o3"
        ++ "o1o1o3o3l3z4o4o3"
        ++ "o1Z1o0o1o2z3o3o3"
        ++ "x o0o0o1o1z2z3o3"
        ++ "x x x o1Z1o0x x "
      }
    , { name = "frog"
      , emoji = "🐸"
      , maze = "bg:134;left:bf0,25;right:09f,10;above:bf0,20;"
        ++ "sz:11,11;off:-1,-2;st:7,7;end:0,-1;mz:"
        ++ "x x x o3o3o3x x x x x "
        ++ "x x x o3o4l4o4o4o4x x "
        ++ "x o4o4l4o4o3o3o3s4o3x "
        ++ "o3s4o4o3o2o2o2o3o3o3o3"
        ++ "o3o3Z3l3o2o1o2o2o2o2s3"
        ++ "o1o2o2s3o1o1o1o1z2o2o2"
        ++ "o1s2o2s2o1o2o2o1o1o1o1"
        ++ "o1o1o2l2o2o2Z2o1o0o0x "
        ++ "o1o0s2o1Z1o0o0l1o0x x "
        ++ "x o0l1o0o0o0o1o1x x x "
        ++ "x x o1o1o1o1o1x x x x "
      }
    ]

levels : Set String -> List Level
levels finishedLevels = List.map
    (\m ->
        { name = m.name
        , emoji = m.emoji
        , maze = m.maze |> Codec.decode |> Maybe.withDefault M.emptyMaze
        , finished = Set.member m.name finishedLevels
        }
    ) mazeDefs
