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
    { emoji: String
    , name: String
    , maze: String
    }

mazeDefs : List MazeDef
mazeDefs =
    [ { emoji = "🐞"
      , name = "ladybug"
      , maze = "sz:3,5;off:0,0;st:0,0;end:2,4;mz:"
        ++ "o0x o0"
        ++ "o0x o0"
        ++ "o0o0o0"
        ++ "o0x o0"
        ++ "o0x o0"
      }

    , { emoji = "🐛"
      , name = "bug"
      , maze = "sz:4,4;off:0,0;st:0,0;end:3,0;mz:"
        ++ "o2o2x x "
        ++ "s2o2o2o2"
        ++ "o1o1o1o2"
        ++ "o0z1o1o2"
      }

    , { emoji = "🐝"
      , name = "bee"
      , maze = "sz:5,5;off:0,0;st:0,0;end:4,3;mz:"
        ++ "x o0z1o1o1"
        ++ "x o0o0o0o1"
        ++ "o1l1o1o0x "
        ++ "o1o0s1x x "
        ++ "o1o0o0x x "
      }

    , { emoji = "🦋"
      , name = "butterfly"
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

    , { emoji = "🐌"
      , name = "snail"
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

    , { emoji = "🐸"
      , name = "frog"
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

    , { emoji = "🦇"
      , name = "bat"
      , maze = "bg:234;left:6bf,15;right:234,25;above:fff,20;"
        ++ "sz:17,18;off:-6,-6;st:0,10;end:-1,0;mz:"
        ++ "x x o4o4z5o5x x x x x x x x x x x "
        ++ "x o3s4o4o4o5o5x x x x x x x x x x "
        ++ "o2o3o3o3o4o4o5o5x x x x x x x x x "
        ++ "o2s3o1o3o3o4z5o5o5x x x x x x x x "
        ++ "o2o2o1s3o2o2o3s5o5x x x x x x x x "
        ++ "x s2o1o2o2z3o3o4l5o4x x x x x x x "
        ++ "x s1o1o1o2o2o3o3o5o4x x x x x x x "
        ++ "x o0o0o1o1o2o2o3o5l5o5o5o5x x x x "
        ++ "x x o0o0o1o1o2o3o3o4o4o4o5o5x x x "
        ++ "x x x o0o0o1o2o2o2s4o3o4o4o5x x x "
        ++ "x x x x x s1o0o0s2s3o3o3o4l5o4o4x "
        ++ "x x x x x o0o0o1o1o2o2l3o2o5o3o4x "
        ++ "x x x x x x o0o0o0o0o2o3o2o5o3o4x "
        ++ "x x x x x x x x o1l1o1o3l3o3o3o4o4"
        ++ "x x x x x x x x o0o0o1o1l2z2z3z4o4"
        ++ "x x x x x x x x x o0o0o0o2x o2o2s4"
        ++ "x x x x x x x x x x o0z1l2z2o2z3o3"
        ++ "x x x x x x x x x x x x o2x x x x "
      }

    , { emoji = "🐒"
      , name = "monkey"
      , maze = "bg:7a0;left:9c3,35;right:c93,10;above:fff,45;"
        ++ "sz:18,14;off:-7,-4;st:1,9;end:2,-4;mz:"
        ++ "x x x x o5z6o6z7o7Z7o6x x x x x x x "
        ++ "x x x o3o5o5l6o5o5o5o6o6x x x x x x "
        ++ "x x o3o3s5o4o6o5o6o5o5o6x x x x x x "
        ++ "o2z3o3z4o4o4s6s5o6o6l6o6o6x x x x x "
        ++ "o2o2o3o2s4o4l5o4o4o4o5l6o5o5o5Z5o4x "
        ++ "x o2l3o2o3o3s5o4o3o3o4s6o4s5o3o3o4x "
        ++ "x x s3o2o2o2s4o4o3o3o4s5o4o4Z4o3l4o3"
        ++ "x x o2Z2o1o1o3o4o3o4o4o4o3s4o2o2s4s3"
        ++ "x x x x s1o1s3s4o3o4o3s4o3o3o2o3o3o2"
        ++ "x x x x o0o1o2o3o3o4o3o3o2o2o2o3o2o2"
        ++ "x x x x x o1l2o1o2s4o3o2o2o1z2l3o2x "
        ++ "x x x x x x o2o1o1o3o3o2o1o1o1o3o2x "
        ++ "x x x x x x x x o1o1z2o2o1o2o2o2o2x "
        ++ "x x x x x x x x x o0o0z1o1o1o1o1x x "
      }

    , { emoji = "🦙"
      , name = "llama"
      , maze = "bg:960;left:ff9,25;right:9ff,10;above:fcf,35;"
        ++ "sz:22,23;off:-8,-8;st:0,14;end:7,-8;mz:"
        ++ "x x x x x x o2z3o3x x x x x x x x x x x x x "
        ++ "x x x x x x o2o1s3x x x x x x x x x x x x x "
        ++ "x x x x x x s2o1o2x x x x x x x x x x x x x "
        ++ "x x o1o1o1x o1o1o2x x x x x x x x x x x x x "
        ++ "x x o1o0l1o0o0o1o2o2z3o3x x x x x x x x x x "
        ++ "o0o0l1o0o1x o0o1o2x x o3x x x x x x x x x x "
        ++ "o0x s1x o1x o0o1l2z2o2o3o3x x x x x x x x x "
        ++ "o0x o0x o1x x x s2x o2o2o3x x x x x x x x x "
        ++ "o0z1l1o1o1x x x o1x o2s2o3x x x x x x x x x "
        ++ "x x o0o0o1z2o2o2l2o2o2o1s3o2o2o2o2x x x x x "
        ++ "x x x o0o0x s2x s1o0o0o1o2o2o2x s2o1o1o1o1x "
        ++ "x x x x o0x s1x o0o0o0o1o1o1o2x o1o1s1x S2x "
        ++ "x x x x o0x o0x x x x x x o1o2x x x o0x o2o2"
        ++ "x x x x o0z1l1o1o1x x x x o1o2Z2o1o1l1o1o1o2"
        ++ "x x x x x x o0o0o1o1x o1Z1l1o0o0o0x o0x s1o2"
        ++ "x x x x x x x o0o0o1x o1x o1o1x o0x o0x o0s2"
        ++ "x x x x x x x x o0o1o1o1Z1o0o0x o0x o0z1l1o1"
        ++ "x x x x x x x x o0o0o0o1x x o0x S1x x x o0x "
        ++ "x x x x x x x x x x o0l1o0z1l1Z1l1o0o0o0o0x "
        ++ "x x x x x x x x x x x o1x x o0x o1x x x x x "
        ++ "x x x x x x x x x x x o1o1o1o0x o1o1x x x x "
        ++ "x x x x x x x x x x x x x s1o0x x S2x x x x "
        ++ "x x x x x x x x x x x x x o0o0o1z2o2x x x x "
      }

    , { emoji = "🐐"
      , name = "goat"
      , maze = "sz:21,19;off:-8,-5;st:-2,13;end:6,-5;mz:"
        ++ "x x x x o3z4o4Z4o3x x x x x x x x x x x x "
        ++ "x x x x o3o3o4x s3x x x x x x x x x x x x "
        ++ "x x x o2z3o3l4o3l3o3x x x x x x x x x x x "
        ++ "x x o1l2o1o1s4o2o2o3o2o2o2x x x x x x x x "
        ++ "x x s1s2o1o3o3o2o2l3o2o1o2x x x x x x x x "
        ++ "x x o0o1o1o3o1o2o1s3s2o1o2o1o1o1x x x x x "
        ++ "o0o0o0o0o1s3o1o2o1o2o1o1o2o1s1o1o1o1x x x "
        ++ "o0z1o1o0o1l2o1o2o1o2x x o2o1o0x x o1x x x "
        ++ "o0o0l1o0o1o2l2o2o1l2o1o1l2o1l1o1Z1l1o0x x "
        ++ "x x o1o0o0o2o1o1o1o2x x s2o0o0o0o1o1o0x x "
        ++ "x x o1o1o1o2o0o0o0s2o1o1o1o0o1o0x x o0x x "
        ++ "x x x x s1o0o1o1o0o1o1o0o0o0o1o0z1o1o0z1o1"
        ++ "x x x x o0o0o1o1o0o0o0o0o1l1o1x x o1o1x o1"
        ++ "x x x x x o0o0o1o1x x x o1o0o0o0o0o0l1o0o1"
        ++ "x x x x x x o0o0o1o1Z1o0o1o1x o1o1o0o1o0o1"
        ++ "x x x x x x x o0o0x o0o0o0o1o0o1o0o0o1o0o1"
        ++ "x x x x x x x x o0z1o1o1l1o1o0o1o0o0o1o0x "
        ++ "x x x x x x x x x x o0o0o0o0o0s1x o1o1o0x "
        ++ "x x x x x x x x x x x x x x o0o0o0o0o0o0x "
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

getLevel : String -> Maybe Level
getLevel levelName =
    levels Set.empty
        |> List.filter(\l -> l.name == levelName)
        |> List.head

getNextUnsolvedLevel : Set String -> Maybe String
getNextUnsolvedLevel finishedLevels =
    mazeDefs
        |> List.filter (\m -> not (Set.member m.name finishedLevels))
        |> List.head
        |> Maybe.map .name
