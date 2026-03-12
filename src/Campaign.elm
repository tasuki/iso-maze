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
      , maze = """sz:3,5;st:0,0;end:2,4;mz:
        o0x o0
        o0x o0
        o0o0o0
        o0x o0
        o0x o0
        """
      }

    , { emoji = "🐛"
      , name = "bug"
      , maze = """bg:560;left:df0,25;right:df0,5;
        sz:4,4;st:0,0;end:3,0;mz:
        o2o2x x
        s2o2o2o2
        o1o1o1o2
        o0z1o1o2
        """
      }

    , { emoji = "🐝"
      , name = "bee"
      , maze = """sz:5,5;st:0,0;end:4,3;mz:
        x o0z1o1o1
        x o0o0o0o1
        o1l1o1o0x
        o1o0s1x x
        o1o0o0x x
        """
      }

    , { emoji = "🦋"
      , name = "butterfly"
      , maze = """above:fff,70;
        sz:9,9;st:3,3;end:7,7;mz:
        x x o2o2o3o3o3o3o3
        x x o2z3o3z4z5o5o3
        x o1s2o1o3o3o3s5o3
        o0o1o1o1z2o2o3s4o3
        o0o0s1o1o1s2o3o3o3
        x o0o0o0o1o1o1s3o2
        x x x o0z1o1z2o2o2
        x x x o0o0o1o1x x
        x x x x o0o0x x x
        """
      }

    , { emoji = "🐌"
      , name = "snail"
      , maze = """bg:770;right:366,20;above:fff,60;
        sz:8,9;st:7,8;end:1,1;mz:
        x x o2z3z4o4z5o5
        x o0o2o2o3z4o4s5
        o0o0S3x o3o3o4o4
        S1o0o3o2o2o3l4o3
        o1z2l3o2o2o3o4o3
        o1o1o3o3l3z4o4o3
        o1Z1o0o1o2z3o3o3
        x o0o0o1o1z2z3o3
        x x x o1Z1o0x x
        """
      }

    , { emoji = "🐸"
      , name = "frog"
      , maze = """bg:134;left:bf0,25;right:2bf,8;above:bf0,30;
        sz:11,11;st:8,9;end:1,1;mz:
        x x x o3o3o3x x x x x
        x x x o3o4l4o4o4o4x x
        x o4o4l4o4o3o3o3s4o3x
        o3s4o4o3o2o2o2o3o3o3o3
        o3o3Z3l3o2o1o2o2o2o2s3
        o1o2o2s3o1o1o1o1z2o2o2
        o1s2o2s2o1o2o2o1o1o1o1
        o1o1o2l2o2o2Z2o1o0o0x
        o1o0s2o1Z1o0o0l1o0x x
        x o0l1o0o0o0o1o1x x x
        x x o1o1o1o1o1x x x x
        """
      }

    , { emoji = "🐭"
      , name = "mouse"
      , maze = """bg:a47;left:a47,40;right:749,40;above:fff,90;
        sz:10,10;st:0,0;end:9,4;mz:
        x x x x o3o2o2o2o2o2
        x x o2z3o3o3l3o3o2S3
        x o1o2o2o2o1l2o1o1o3
        o0o1s2o1o2o1s2o2o2o3
        o0o1s1o1o2o1o1o2o1s3
        o0o1o0o1l2o1o2o2o1o2
        o0l1o0o1o2o2o2o1o1x
        o0o1o1o1o2s2o1o1x x
        o0o1s1o0z1o1o1x x x
        o0o0o0o0o0o0x x x x
        """
      }

    , { emoji = "🐹"
      , name = "hamster"
      , maze = """bg:863;above:fff,60;
        sz:11,11;st:1,2;end:9,10;mz:
        x x x o1o2z3o3o3o3o3x
        x x o0s1o2o1z2o2o2o3x
        x x o0o0o2l2o2Z2o1s3o1
        o0o0o0o1o1o1o1o2o1o2o1
        o0o1z2l2o2o2o1l2o1o2o1
        o0o1o0s1s2o0o0s2o0z1o1
        o0o1o0o0l1o0z1o1o0o0x
        o0s1o1o1o1o0o0o0o0x x
        o0o0z1o1o0o0o1x x x x
        x o0o0o1l1o1o1x x x x
        x x o0o0o0o0x x x x x
        """
      }

    , { emoji = "🐥"
      , name = "chick"
      , maze = """above:fff,60;
        sz:9,11;st:0,0;end:8,10;mz:
        x x x x o2o2z3z4o4
        x x x x o2o3o3o3o3
        x x x x o2o2o2o2o3
        x x o3o3o3o3o3l3o3
        o2o2o3s3o3s3o2o2o3
        s2o2o2s2o3o2o2o3o3
        o1o1l2o1o3o3o3o3s3
        o1z2o2Z2o1o1o1o2o2
        s1o1Z1o0o0z1o1o2x
        o0o1o1l1o1o1o1x x
        o0o0o0o0o0x x x x
        """
      }

    , { emoji = "🐿️"
      , name = "squirrel"
      , maze = """above:fff,60;
        sz:10,10;st:2,0;end:8,9;mz:
        x x o1o1o2o2o2o2o3x
        x x S2o1z2z3o3o2o3x
        x o1o2o2o2o2l3o2o3o3
        x o1o1o1s2o3o3o2z3o3
        o2l2o2o1s1s3o3S3o4o4
        o2o1o2o1o0o2o2o3z4o4
        o2o1o2o1l1z2o2o3o3x
        o2o1s2o1S1o2o2o3x x
        o1o1s1o1o1o2x x x x
        o1Z1o0z1o1x x x x x
        """
      }

    , { emoji = "🦎"
      , name = "lizard"
      , maze = """bg:cb9;left:884,35;right:853,15;above:dfd,90;
        sz:11,10;st:1,1;end:8,6;mz:
        x x x o1o2o2z3o3o3o3x
        x x x o1s2o3z4l4o4o4o4
        x x x o1o1s3o2o3z4z5o5
        o2o2o2o1z2o2o2o4o5o5o5
        o1l2o1z2z3l3z4o4o3o3x
        o1o2o2o1o2o2o2z3o3x x
        s1o2s2o1o2o1o2x x x x
        o0o0s1o1s2o1o2x x x x
        x o0o0o1o1o1x x x x x
        x x o0z1o1x x x x x x
        """
      }

    , { emoji = "🦔"
      , name = "hedgehog"
      , maze = """sz:10,11;st:0,0;end:8,9;mz:
        x x x x o4o4o4o4o4x
        x x x o1o4o3o3o3o4o4
        x x o1o1o0o3s3S4o3o4
        o1o1o1o0o0o3s2o4l4o4
        o1o0o0o0o2o2o1o1o3o3
        o1o0z1o1o1l2o1z2z3o3
        o1o0o2o2o2o2o2o2o3o3
        o1l1o1o1o1o1o2s2x x
        o1o0o0o0l1o0z1o1x x
        s1o0o1o1o1o0o0x x x
        o0o0o0z1o1o0x x x x
        """
      }

    , { emoji = "🐰"
      , name = "rabbit"
      , maze = """sz:11,11;st:0,2;end:10,5;mz:
        x x x x o0o1o1o1o0o0x
        x x x x o0o0o0l1o0z1o1
        x x o0o0S1o1o0S2o1o0o1
        x x S1o0o1o1o2o2o1o0o1
        o1o1o1o0o0o1o1o2o1o0o1
        o1o0o0o0z1z2l2o2o1o0o1
        o1o1o1l1o1o1s1o2o1o0x
        s1o0o0o0o0o1o0s2s1o0x
        o0o0x x o0l1o0o1o0o0x
        x x x o0o0o1o0o0o0x x
        x x x o0z1o1o0x x x x
        """
      }

    , { emoji = "🦆"
      , name = "duck"
      , maze = """sz:11,16;st:0,0;end:8,15;mz:
        x x x o3z4o4o4o4o4x x
        x x x s3o3o3o3o3o4o4x
        x o1z2o2o4l4o4o3o3o4o4
        x o1o1o3o3o3o4o4o3o3o4
        x o0o2o2o2o3o3o4o4o3o4
        x o0o2o1l2o1o3o3o4o4s4
        o0o0o2o1o2o2o2o3o3l4o3
        o0z1l2o1o2o1o2o3z4o4o3
        o0o0o2x o2o1o2s3o2o2s3
        o0o1l2o1l2o1o2s2o1o2o2
        o0o1s2o0o2o1o2o1o1o1x
        o0o1s1S1o2o1o1o1o0x x
        o0o1o0o1s2o0o0o0o0x x
        o0l1o0o1o1l1o1o1x x x
        o0s1o0l1o0o0x x x x x
        o0o0z1o1x x x x x x x
        """
      }

    , { emoji = "🦇"
      , name = "bat"
      , maze = """bg:234;left:6bf,15;right:234,25;above:def,40;
        sz:17,18;st:6,16;end:5,6;mz:
        x x o4o4z5o5x x x x x x x x x x x
        x o3s4o4o4o5o5x x x x x x x x x x
        o2o3o3o3o4o4o5o5x x x x x x x x x
        o2s3o1o3o3o4z5o5o5x x x x x x x x
        o2o2o1s3o2o2o3s5o5x x x x x x x x
        x s2o1o2o2z3o3o4l5o4x x x x x x x
        x s1o1o1o2o2o3o3o5o4x x x x x x x
        x o0o0o1o1o2o2o3o5l5o5o5o5x x x x
        x x o0o0o1o1o2o3o3o4o4o4o5o5x x x
        x x x o0o0o1o2o2o2s4o3o4o4o5x x x
        x x x x x s1o0o0s2s3o3o3o4l5o4o4x
        x x x x x o0o0o1o1o2o2l3o2o5s4o4x
        x x x x x x o0o0o0o0o2o3o2o5o3o4x
        x x x x x x x x o1l1o1o3l3o3o3o4o4
        x x x x x x x x o0o0o1o1l2z2z3z4o4
        x x x x x x x x x o0o0o0o2x o2o2s4
        x x x x x x x x x x o0z1l2z2o2z3o3
        x x x x x x x x x x x x o2x x x x
        """
      }

    , { emoji = "🐒"
      , name = "monkey"
      , maze = """bg:7a0;left:9c3,33;right:c93,12;above:fff,90;
        sz:18,14;st:8,13;end:9,0;mz:
        x x x x o5z6o6z7o7Z7o6x x x x x x x
        x x x o3o5o5l6o5o5o5o6o6x x x x x x
        x x o3o3s5o4o6o5o6o5o5o6x x x x x x
        o2z3o3z4o4o4s6s5o6o6l6o6o6x x x x x
        o2o2o3o2s4o4l5o4o4o4o5l6o5o5o5Z5o4x
        x o2l3o2o3o3s5o4o3o3o4s6o4s5o3o3o4x
        x x s3o2o2o2s4o4o3o3o4s5o4o4Z4o3l4o3
        x x o2Z2o1o1o3o4o3o4o4o4o3s4o2o2s4s3
        x x x x s1o1s3s4o3o4o3s4o3o3o2o3o3o2
        x x x x o0o1o2o3o3o4o3o3o2o2o2o3o2o2
        x x x x x o1l2o1o2s4o3o2o2o1z2l3o2x
        x x x x x x o2o1o1o3o3o2o1o1o1o3o2x
        x x x x x x x x o1o1z2o2o1o2o2o2o2x
        x x x x x x x x x o0o0z1o1o1o1o1x x
        """
      }

    , { emoji = "🦙"
      , name = "llama"
      , maze = """bg:960;left:ff9,25;right:9ff,10;above:fef,70;
        sz:19,20;st:7,19;end:14,0;mz:
        x x x x x o2z3o3x x x x x x x x x x x
        x x x x o0s2o1o3x x x x x x x x x x x
        x x x x o0o1o1s3x x x x x x x x x x x
        x o1o1o1o0o0o1o2o2z3o3x x x x x x x x
        x o1o0l1o0o0o1o2x x o3x x x x x x x x
        o0l1o0o1o1o0o1l2z2o2o3o3x x x x x x x
        o0o1x x o1o0o1s2x o2o2o3x x x x x x x
        o0l1z1o1o1z2o2l2o2o2s2s3x x x x x x x
        x s1o0o0o0o0o2o1o1o0o1o2x x o1Z1o0o0x
        x o0o0x x o0s2x o1o0o1o2Z2o1o1o1o1l1o1
        x x x x x o0l1o0o1o0l1o0o0o0o0o0o1o0S2
        x x x x x x s1S1x o0o1o1o1o1o1o0o1o0o2
        x x x x x x o0o1o1o0S2x x S2o1o0o1o0o2
        x x x x x x o0o0o1S1o2x x o2o1o0o1o0s2
        x x x x x x x o0o1o1o2o2o2o2x o0o1l1o1
        x x x x x x x o0o0o1o1o1o1o1x S1x o0x
        x x x x x x x x o0o0l1o0o0l1o0l1o0o0x
        x x x x x x x x x x o1o1s1o1o1x x x
        x x x x x x x x x x x x s1o0o1x x x x
        x x x x x x x x x x x x o0o0o1x x x x
        """
      }

    , { emoji = "🐐"
      , name = "goat"
      , maze = """above:fff,80;
        sz:21,19;st:6,18;end:13,1;mz:
        x x x x o3z4o4Z4o3x x x x x x x x x x x x
        x x x x o3o3o4x s3x x x x x x x x x x x x
        x x x o2z3o3l4o3l3o3x x x x x x x x x x x
        x x o1l2o1o1s4o2o2o3o2o2o2x x x x x x x x
        x x s1s2o1o3o3o2o2l3o2o1o2x x x x x x x x
        x x o0o1o1o3o2o2o1s3s2o1o2o1o1o1x x x x x
        o0o0o0o0o1s3o1o2o1o2o1o1o2o1s1o1o1o1x x x
        o0z1o1o0o1l2o1o2o1o2x o2o2o1o0o0o0o1x x x
        o0l1z1o1o1z2o2l2o2o1l2o1l2o1o1l1o1Z1l1o0x x
        x x o1o0o0o2o1o1o1o2x s2o0o0o0o0x o1o0x x
        x x o1o1o1o2o0o0o0s2o1o1o0o1o1o0x o0o0x x
        x x x x s1o0o1o1o0o1o1o0o0o0o1o0z1o1o0z1o1
        x x x x o0o0o0o1o0o0o0o0o1l1o1x x o1o1x o1
        x x x x x x o0o1o1o1o1x o1o0o0o0o0o0l1o0o1
        x x x x x x o0o0o0o1Z1o0o1o1x o1o1o0o1o0o1
        x x x x x x x x o0x o0o0o0o1o0o1o0o0o1o0o1
        x x x x x x x x o0z1o1o1l1o1o0o1o0o1o1o0x
        x x x x x x x x x x o0o0o0o0o0s1x o1o0o0x
        x x x x x x x x x x x x x x o0o0o0o0o0x x
        """
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
