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

-- https://emojipedia.org/nature has an overview of emoji

mazeDefs : List MazeDef
mazeDefs =
    [ { emoji = "🐞" -- 1
      , name = "ladybug"
      , maze = """sz:3,5;st:0,0;end:2,4;mz:
        o0x o0
        o0x o0
        o0o0o0
        o0x o0
        o0x o0
        """
      }

    , { emoji = "🐛" -- 2
      , name = "bug"
      , maze = """bg:560;left:df0,25;right:df0,5;
        sz:4,4;st:0,0;end:3,0;mz:
        o2o2x x
        s2o2o2o2
        o1o1o1o2
        o0z1o1o2
        """
      }

    , { emoji = "🐜" -- 3
      , name = "ant"
      , maze = """
        sz:5,4;st:1,1;end:4,3;mz:
        o0z1z2o2o2
        o0o0o1o1o1
        x o0s1o0s1
        x o0o0o0o0
        """
      }

    , { emoji = "🐝" -- 4
      , name = "bee"
      , maze = """sz:5,5;st:0,0;end:4,3;mz:
        x o0z1o1o1
        x o0o0o0o1
        o1l1o1o0x
        o1o0s1x x
        o1o0o0x x
        """
      }

    , { emoji = "🦋" -- 5
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

    , { emoji = "🐌" -- 6
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

    , { emoji = "🐸" -- 7
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

    , { emoji = "🐭" -- 8
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

    , { emoji = "🐹" -- 9
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

    , { emoji = "🐥" -- 10
      , name = "chick"
      , maze = """bg:990;left:cc0,50;right:cc0,7;above:fff,60;
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

    , { emoji = "🐿️" -- 11
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

    , { emoji = "🦎" -- 12
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

    , { emoji = "🦔" -- 13
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

    , { emoji = "🐰" -- 14
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

    , { emoji = "🐦" -- 15
      , name = "bird"
      , maze = """
        sz:13,7;st:2,0;end:11,6;mz:
        o5z6z7o7o7o7o7z8z9o9o9o9x
        s5o4o3o6o6o6o6o6o6o6o6s9x
        o4o4o3o3o3z4z5o5o5o5o6s8o6
        s4o2o1z2l3o2o4l5z5z6o6l7o6
        s3o2o1o1s3o2s4o5z6z7o7o7o6
        o2o2s1o1s2o2o3o3o4z5z6o6o6
        x x o0z1o1z2z3o3x x x x x
        """
      }

    , { emoji = "🦆" -- 16
      , name = "duck"
      , maze = """
        bg:0cf;
        sz:11,16;st:0,0;end:8,15;mz:
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

    , { emoji = "🐓" -- 17
      , name = "rooster"
      , maze = """
        left:ff0,30;right:0ff,30;above:f0f,90;
        sz:16,15;st:0,0;end:14,13;mz:
        x x x o3o3o3o3Z3o2o2o2o2x x x x
        x x o1o1o1o1o1o1o2z3o3o2o4o4o5o5
        x x o1o2o2o2o2l2o2o3o3o2o2o4o4s5
        x x o1o1o1o1o1o1o2o2o2z3l3o3o4s4
        x o2o2o2o2o2o2o1o1o1o1o1o2o3o4o3
        x s2o0o1o1z2o2o1o2o2o2o2o2o3o4o3
        o0l1o0o1o0o0z1o1l2o1o2o3o3o3l4o3
        o0o1o0o1o0z1z2o2o2o1o2z3z4o4o4x
        S1s1o0o1o0o1o1o1o2o1o2o2o2x x x
        o1o0o0o1o0o0o0o1o2o1o1o1x x x x
        o1l1o1o1o1o1S1o1o2o2x x x x x x
        o1o0o0o0o0o0o1o1o1o2x x x x x x
        s1o0o1o1o1o0o0z1o1x x x x x x x
        o0o0l1o0o1o1o1x x x x x x x x x
        o0z1o1o0o0o0x x x x x x x x x x
        """
      }

    , { emoji = "🦚" -- 18
      , name = "peacock"
      , maze = """
        left:fc0,40;right:0cf,40;above:fff,55;
        sz:18,18;st:1,1;end:15,15;mz:
        x x x x x x x x x o7o7o7o7o7o7o7x x
        x x x x x x x o7o7o7Z7Z6o5z6z7o7x x
        x x x x x x o6o6o6o6o6o6o6o6o6o7o7o7
        x x x x x o5o5o5o5o5o5l6o5o5o6o6s7o7
        x x x o2o2o5o6o6o6o6o6o6o6o5o5o6s6o7
        x x x o2o4o5o6Z6Z5o4o4o4o6o6o5l6o5o7
        x x x s2o4o5o5o5o5l5o5o4o4o6o5o6S6o7
        x x o1o1o4o4o4o4o4o4o5o5o4o6o5o6S7o7
        x o1o1o5o5o5o5o5o5o4o4o5o4o6o5o6o7o7
        o1o1o4o4o4o4o4x o5o5l5o5S5o6o5o6o7x
        o1o3o3o3o3o3o4o4o4l5o4o5S6o6o5o6o7x
        o1s3o2Z2o1o3l4z4z5o5o4o5o6o6o5o6x x
        s1o2o2o2o1o3o4o4o4o5o4o5o5o5o5x x x
        o0o2s2o2o1o1o1s4o4o5o4o4o4o2x x x x
        o0z1o1o2o2o2S2o3o4o5o1z2o2o2x x x x
        o0o1o1o1z2o2o2o3o4o1o1x x x x x x x
        o0o0o1s1o2o2z3o3o1o1x x x x x x x x
        x o0o0o0o0z1o1o1o1x x x x x x x x x
        """
      }

    , { emoji = "🦢" -- 19
      , name = "swan"
      , maze = """
        bg:0cf;left:fc9,40;right:0cf,10;above:fff,65;
        sz:19,16;st:1,0;end:16,15;mz:
        x x x x x x x x x x x o6o6o6o6o6o6x x
        x x x x x x x o3z4o4o5l6o5o5o5o5o6o6o6
        x x x x x o2o2l3o2o4o5o6o6o6o6o5o5o5o6
        x x x x o1l2o1o3o2o4l5z5o5s6o6o6o6o5x
        x x x x o1o2o1o3o2x s5o5o5o5o5o5o6o5x
        x x x o1o1o2o1o3o2o2o4o4o4o4o4o5o6o5x
        x x x o1o2o2o1o3o3o2o2z3o3o3o4o5o6x x
        x o1o1o1o2o1o1o1o3o3o3z4o4l4o4o5x x x
        x o1z2o2o2o2o2l2o2o2l3o2z3o3o4o5x x x
        x o1o0o0z1o1o1o1o1o1o3o4Z4o3o4x x x x
        x o1o0z1z2o2o2o2o2o1s3s4o2o2o4x x x x
        x o1o0o0z1o1o1o1o2l2o2o3Z3o2x x x x x
        x o1o0o1z2o2o2o1o1o1o2Z2o1x x x x x x
        o1o1o0o1o1o1o2o2o2o1Z1o0x x x x x x x
        s1o0o0l1o0o1o1o1s2x x x x x x x x x x
        o0o0z1o1o0o0o0o1o1x x x x x x x x x x
        """
      }

    , { emoji = "🦩" -- 20
      , name = "flamingo"
      , maze = """
        bg:e6e;left:e6e,30;right:93f,10;above:fff,66;
        sz:16,19;st:0,1;end:15,17;mz:
        x x x x x x x x o7o7o7o7o7o7o7x
        x x x x x x x o5z6o6o6o6l7o6o7o8
        x x x x x x o4l5o4o5o5o5o7S7o7o8
        x x x x x x o4o5o4l5o4o4o7S8o7o8
        x x x x o4o4o4o5o5o5o5o4o7o8o7o8
        x x x o3o4o3l4o3z4z5o5o5s7o8o8o8
        x x x o3s4s3o4o4o4o4o4o5o6o6o6s8
        x x x o3s3o2o2o2o2s4o4o5o5z6o6o7
        x x x o3l3o3o3o3o2o3o4o4o5o5o6o7
        x x o2o2o2o2o2o3o2o3o3o4o4o5l6o5
        x o1l2o1z2z3l3o3o2o2o3z4o4o2s6o5
        x o1o2o2o2o2o2o3o3o2o2o2o2o2s5o5
        x S2o2o1o1o1z2o2o3Z3o2o3o3o3o4o5
        x o2o2o1z2z3o3o2o3o2o2o3o4o4o4x
        o1o1o2l2o2o3o3o2o3o3o3o3x x x x
        s1o1s2o1o2o2o3o2o2x x x x x x x
        o0o1s1o1o1o2o3Z3o2x x x x x x x
        o0l1o0z1o1o2o2x x x x x x x x x
        x o1o1o1o1x x x x x x x x x x x
        """
      }

    , { emoji = "🦜" -- 21
      , name = "parrot"
      , maze = """
        bg:0cc;left:ff0,30;right:f09,30;above:6ff,100;
        sz:16,15;st:1,2;end:15,12;mz:
        x x x x x x x o4o4o4o4x x x x x
        x x x x x x o3o3o3o3o4o4o4o4x x
        x x x x x o2l3o2o2o2z3l4o3o4o4o4
        x x x x o3o3o3o3l3o3x o4o3o3o3o3
        x x x o2o2o2o2o2o2o3o3o4o4o4o4o3
        x x o1o1o1o1l2o1o2o2l3z3o3o3o4o3
        x x o1z2o2o2o2o1z2z3o3x o4l4o4o3
        x o2z3o3l3o3o2o2o2o3o3o4o4o3o3o3
        o1l2o1z2o2o3o3o3o2S4o3o4x x x x
        o1s2S2o1o1o1o1o3o2o4o3o4o3x x x
        o1s1o2o2o2o2o1s3S3o4l4o4o3x x x
        o1l1o1o1o1o2o1o2o3o4o3o3o3x x x
        x o0l1o0o1o1o1o2o3o4o2o2x x x x
        x S1o1o0o0z1z2o2o2o2o2x x x x x
        x o1o1x x x x x x x x x x x x x
        """
      }

    , { emoji = "🐧" -- 22
      , name = "penguin"
      , maze = """
        bg:069;right:069,20;
        bg:069;right:069,20;sz:18,17;st:0,0;end:17,15;mz:
        x x x x x x o4o4o4o4o4o4o4o4o4o4o4x
        x x x x x o1z2z3o3l4z4z5o5o5o5o5l5o5
        x x x x x o1o3o3z4o4o4o4o4o4o4o4s4o5
        x x x x x o1z2o2o2z3o3o3o3o3o3o4o3s5
        x x o1o2o2z3o3o3o3o4o4o4o4o4o3o4o3o4
        x x o1o2o1z2o2o2l3z3o3o3o3o4o3o4o3o4
        x x s1o2o1o0z1o1o3o4o4o4o3o4o3s4o3o4
        x x o0o2o1o0o0S2o3o4o3o4o3o4o3o3o3o4
        x x o0o2o1l1o1o2o3o4o3o4o3o4o4o3o4o4
        x x o0o0o0o0S2s2o3s4o3o4o3o3o4o3o4x
        x x o0z1o1o1o2o1o3s3o3o4o4l4o4o3o4x
        x o0o0o2o2l2o2o1o3o2o3o3o2o3o3o3x x
        x o0o1l2o1o1o2o1s3o2z3o3o2x x x x x
        o0o0o1s2o1o0o2l2o2o2o1z2o2x x x x x
        S1o0o1s1o1o0z1o1o0z1o1x x x x x x x
        o1o0l1o0o1o0o0o0o0x x x x x x x x x
        o1o1o1o1Z1o0x x x x x x x x x x x x
        """
      }

    , { emoji = "🦥" -- 23
      , name = "sloth"
      , maze ="""
        bg:896;right:cfc,20;
        sz:18,13;st:1,0;end:17,11;mz:
        x x x x x x x x x o5z6z7o7o7o7z8o8x
        x x x x x o5o5o5o5o5o6o6o6o6o6o6o8o8
        x x x x x o5z6o6o6o6o6o5o5z6z7l7o7o8
        x x x x o4o4z5o5o5o5o6l6o6o6o6o6o7o8
        x x x o3o3o3o3z4o4o5o5o5o6o5o5o6o7o8
        x x o2o2o3s3o4o4o4o4o4l5o4z5o5o6o7o8
        x x s2o2z3l3o3o3o3o3o4o5o4o5o5o6o7o8
        x x o1o1z2o2o2o2o2o3o4s5o6o5S6s6o7o8
        x o0o0o0o0o0z1o1o2o3o4o4o6o5l6o5o7x
        o1l1o1o1o1z2o2o1o2o3o3o4o6l6o6o7o7x
        o1o0o0o0o0o0o2o1o2s3o3o4o5o5x x x x
        o1l1o1o1o0S1o2l2o2o2o3o4o5x x x x x
        x o0z1o1o1o1o1o1x x x x x x x x x x
        """
      }

    , { emoji = "🐱" -- 24
      , name = "cat"
      , maze ="""
        sz:19,16;st:0,0;end:18,15;mz:
        x x x x x x x x x x x x x x o4z5z6o6o7
        x x x x x x o3o3z4o4o4o4o4z5l5o5o6o6o7
        x x x x x o4l4o4z5o5o5Z5Z4o3o4o5o6o5o7
        x x x x x x o3o3o3o3o3z4o4o3o4o5l6o5s7
        x x x x x o1o1o1o1z2o2o2o4o3o4o5o6o5o6
        x x x x o0z1z2o2l2z3o3o2s4o3o4s5o6o5o6
        x x x x o0o0z1o1o1o3o3l3o3o3o4s4o6l6o6
        x x o0z1o1z2o2o2l2o2o4S3o3o3l4o3o3o5o5
        x x o0o0o0o0o0o0o1o2o4S4o3o4o4o4o3o3x
        x o1o0z1o1o1o1o0o1o2o4o4o3o3z4o4o4x x
        x o1o1o1o1o0l1o0o1o2z3o3o4o4z5o5o4x x
        o0o0o0o0o0o0s1o1o1o2o3o3z4o4o5o5x x x
        o0o1o1o0z1o1o0o1o2o2o2o2Z2o1x x x x x
        o0o1s1o1o1o1o0o1o2o1o1o1o1o1x x x x x
        o0s1o0l1o0o0o0o1o2x x x x x x x x x x
        o0o0o0o1o1o1o1o1x x x x x x x x x x x
        """
      }

    , { emoji = "🦊" -- 25
      , name = "fox"
      , maze ="""
        bg:c60;left:c60,40;right:cfc,10;above:fff,90;
        sz:16,14;st:0,0;end:14,12;mz:
        x x x o4o4o4z5o5o5o5o5o5o5o5z6o6
        x x o3o3o3o3o3z4o4o4o4l5o4o5o7o6
        x x o3o2o2o2o3o5o5o5o5s5o5o5o7o6
        x x o3l3o3o2o3z4o4l5o4o4o5o6o7o6
        x o2o2o2o3z4o4o4z5o5o5o4o5o6l7o6
        o1l2o1o2z3o3o3o3z4o4o5o4o5o5s7x
        o1s2o1o1o1o1o1z2o2o4o5o4o4o5s6x
        o1s1o1o0o0o0o1o2o2o4s5o3o4o5o5x
        o1o0o1o0o1o0o1o2z3o3s4o3o4x x x
        o1o0l1o0o1o0S2o3o3o3s3o3o4x x x
        o1o1o1o0o1o0o2o3o2o2o2o3x x x x
        o0l1o0o0o1o0o2o3o2o3o3o3x x x x
        o0s1o1o1o1S1o2o3l3o3x x x x x x
        o0o0o0z1o1o1o2o2o2x x x x x x x
        """
      }

    , { emoji = "🐕" -- 26
      , name = "dog"
      , maze ="""
        sz:14,19;st:1,1;end:10,15;mz:
        x x x x x x x x x x x o7o7x
        x x x x x o5o5o5z6o6o7o7s7x
        x x x x o4l5o4o4o4o6o7x s6x
        x x x x o4o5o5o5o4o6o7x o5o5
        x x o3o3l4z4o4o5o4o6o6o6o6o5
        x o4o4o3S5o4o4o5o4o4o4z5l6o5
        x s4o4o3o5o5o5o5o5o5o5o6o6x
        o3o3o4o3o4Z4o3o5o6o6o6o6o5x
        s3o3o4l4o4o3o3o5o5o5o6s6o5x
        o2o2s4s3o4o3o4o4o4o5o5o5o5x
        o2s2o3o2o4o3o3o3l4z4z5o5x x
        o1o1s3s2o4o4o4o4o4o5o5o5x x
        s1o1o2o1o2o2o2o2z3o3o3x x x
        o0o0z1o1o1o1l2o1o1o1o3x x x
        o0o1o1o2o2o2o2o2o2o1o3x x x
        o0s1o1l2o1o1o1o1o2o1x x x x
        o0o0o2o2o2o2o2o1o2x x x x x
        x o0o0z1o1o1l2o1x x x x x x
        x x o0o0z1z2o2x x x x x x x
        """
      }

    , { emoji = "🐒" -- 27
      , name = "monkey"
      , maze ="""
        bg:7a0;left:9c3,33;right:c93,12;above:fff,90;
        sz:18,17;st:3,3;end:17,16;mz:
        x x x x x x x x o5z6o6o6o6o6o6z7o7o7
        x x x x x x x x o5o6o6o7o7o7o7o7o7s7
        x x x x x x o6o6o6o6o7o7o6o6o6o6o6s6
        x x x x x x o6z7o7o7o7o6o6o5l6o5o6o5
        x x x x x o4z5o5z6o6o6o6s6s5o6o5o6o5
        x x x o1z2o2z3z4z5o5o5o5s5o4s6s5o6o5
        x x x s1o2o2o4z5o5o5o4o5s4o4o5o4o6o5
        x x o1l1o1o2z3z4o4o4o4o4l4o4o5o4o6o5
        x x o1o0o1o1z2z3o3o3o3o4o3o4o5o4o6o5
        o0o0l1o0o0o0o0z1z2o2o3o4o3o4o5l5o5o5
        o0z1o1o1o1o1o1o1o2o2o3o4o3o4o4o4o3x
        x x s1o0o0o0o0S2S3o3o3o4o3o3o3o3o3x
        x x o0o0z1o1o0o2o3s3o4o4o3x x x x x
        x x x o0o0o1o0o2o3o2s4o3o3x x x x x
        x x x x o0o1o0o2o3s2o3o3x x x x x x
        x x x x o0l1o0s2o1o1o3x x x x x x x
        x x x x x o1o1o1o1o3o3x x x x x x x
      """
      }

    , { emoji = "🦉" -- 28
      , name = "owl"
      , maze = """
        bg:234;left:6bf,15;right:234,25;above:def,40;
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

    , { emoji = "🦇" -- 29
      , name = "bat"
      , maze = """
        bg:234;left:6bf,15;right:600,7;above:def,40;
        sz:17,17;st:6,15;end:5,5;mz:
        x x o4o4z5o5x x x x x x x x x x x
        x o3s4o4o4o5o5x x x x x x x x x x
        o2o3o3o3o4o4o5o5x x x x x x x x x
        o2s3o1o3o3o4z5o5o5x x x x x x x x
        o2o2o1s3o2o2o3s5o5x x x x x x x x
        x s2o1o2o2z3o3o4l5o4x x x x x x x
        x s1o1o1o2o2o3o3o5o4x x x x x x x
        x o0o0o1o1o2o2o3o5l5o5o5o5x x x x
        x x o0o0o1o1o2o3o3o4o4o4o5o5x x x
        x x o0z1z2l2o2o2o2s4o3o4o4o5x x x
        x x x x x s1o0o0s2s3o3o3o4l5o4x x
        x x x x x o0o0o1o1o2z3o3o5o5o4x x
        x x x x x x o0o0o0o0o1o3o3o3o4o4x
        x x x x x x x x o1l1o1o3s3o3o3o4o4
        x x x x x x x x o1o0o1o1l2z2z3z4o4
        x x x x x x x x x o0o0o2o2x o2o2s4
        x x x x x x x x x x o0z1o1z2o2z3o3
        """
      }

    , { emoji = "🐐" -- 30
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
        o0o0l1o0o1o2l2o2o1l2o1l2o1o1l1o1Z1l1o0x x
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

    , { emoji = "🐑" -- 31
      , name = "sheep"
      , maze ="""
        sz:18,18;st:6,17;end:11,0;mz:
        x x x x o3z4o4o4Z4o3x x x x x x x x
        x x x o1s3o2s4o2o2s3x x x x x x x x
        x x o2l2o2o2o3o2o2o2o2x x x x x x x
        x o2o2o1s2o2o3l3o3o3o2x x x x x x x
        o2o2o1o1o1o2o3o2o2o3o2z3o3x x x x x
        o2Z2o1o0o0o2o3o3o2o3s2o3o3o3o3x x x
        x x o1o0S1o2o2o2s2s3o1o3o2z3o3o2o2x
        x x o1o0o1o1o1o2l2o2o1z2o2o1o1o1l2o1
        x x x o0o0o0o1o1o1o2o2o1o1o1o2o2o2o1
        x x x x x o0o0o0o1o1o1o1o0o0z1z2o2o1
        x x x x x x x o0o1o0o0o0o0o1o1o1l2o1
        x x x x x x x o0l1o0o1o1o1o1o2o2o2x
        x x x x x x x o0o1o1o1o0o0o0z1l2o1x
        x x x x x x x o0o0o0o0o0o1o1z2o2o1x
        x x x x x x x x o1o1o1o1o1o0z1o1o1x
        x x x x x x x x x o0o0o0l1o0o1o1x x
        x x x x x x x x x x o1o1o1o1Z1o0x x
        x x x x x x x x x x x o0o0o0o0o0x x
         """
       }

    , { emoji = "🐷" -- 32
      , name = "pig"
      , maze ="""
        sz:16,18;st:5,17;end:12,1;mz:
        x x x o3o3o3o3o3o3x x x x x x x
        x x o1s3o2o3o2o2l3o2x x x x x x
        o1o1o1o2o2l3o2o1s3o2o1o1o1x x x
        o1z2z3o3o3o3o1o1o2o2l2o2o1x x x
        o1o1o1o1o1o1o1o2o2o1o1o2o1o2x x
        S2o2o2o2o2o2o1o1o1o1o2o2o1S3x x
        o2o2o1o1o1o2o2o2l2o2o1o1o1o3Z3o2
        o1o1o1o2o1o1o1o1o1s2o1o2o3o3x s2
        o1z2o2o2o2o2o2l2o2o1o1o2o1s3o1o1
        x x s2o1Z1o0o0o1o2o2o2o2o1o2o1o0
        x x o1o1o0o1o0o1o1o1o1o1o1o2o1o0
        x x x s1o0o1o0l1o0o0o0o0o0o2o1o0
        x x x o0o0o1o1o1o0o1o1o1z2o2o1o0
        x x x x x x o0o0o0o1o0o0o0o1o1o0
        x x x x x x S1o1o1o1o0o1o0l1o0o0
        x x x x x x o1o1Z1o0o0o1o1o1o0x
        x x x x x x x x x x x x o0s1x x
        x x x x x x x x x x x x o0o0x x
        """
      }

    , { emoji = "🦙" -- 33
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
        x x x x x x x x x x o1o1o1s1o1o1x x x
        x x x x x x x x x x x x s1o0o1x x x x
        x x x x x x x x x x x x o0o0o1x x x x
        """
      }

    , { emoji = "🦘" -- 34
      , name = "kangaroo"
      , maze ="""
        sz:17,16;st:6,15;end:8,0;mz:
        x o5o5z6o6z7o7x x x x x x x x x x
        x o5o6o6o6o6l7o6x x x x x x x x x
        x s5o6z7l7o7o7o6x x x x x x x x x
        x o4o4o4o6o6Z6l6o5o5o5x x x x x x
        x o4o3l4o3z4o4o6o6o6l6Z6o5x x x x
        o2o4o3o4o3o3o4s6o5o5o5x s5o3Z3o2x
        o2o4o3o4o4o3o4s5o5o3o3o3l4o3o2o2x
        o2o4o3x o4o3o4o4o3o3o4o4o4o3o2z3o3
        o2o4l4o4o4o3o2z3o3o4o4o3l4o3o2o3o3
        o2z3o3Z3o2l3o2o3z4o4o3o3o4o3o2o3o2
        x o1o1o1s2o3o2l3o2s4s3x s4s3s2o3o2
        x x x o1o1o3o2o3o2o3l3o3o3o2o1o3o2
        x x x x x o3o2o3l3o3s2o2o2o2o2o3o2
        x x x x x o1o2o2o2o1o1o1o1o1z2l3o2
        x x x x x o1o1o1o1o1o2o2o2z3o3o3o2
        x x x x x x x x o0o0o0z1o1o1o1z2o2
        """
      }

    , { emoji = "🐨" -- 35
      , name = "koala"
      , maze ="""
        sz:19,20;st:3,19;end:17,0;mz:
        x x o4o4o4x x x x x x x x x x x x x x
        x o3l4o3o4o4x x x x x x x x x x x x x
        o3o3o4o3s4o4Z4o3o3x x x x x x x x x x
        o3z4o4o3o3s4o2s3o3x x x x x x x x x x
        o1z2o2o2o2l3o2o2o3o2o2o2o2o2x x x x x
        o1o1o3o3l3o3o3o3o3o2o3o2S3o2x x x x x
        o1o2l3o2o2o2o2o2o2o2o3o2o3o2x x x x x
        o1s2o3o1o1o1o1o1z2z3o3s2o3o2z3o3x x x
        o1o1o1o1o0o0o0o0z1o1o1o1s3s2o3o3x x x
        x x x x o0o1o1o1z2o2o2o2o2o1s3o3o2o2x
        x x x x o0o0o0o0o0o0o1o1o1o1s2o3s2o2x
        x x x x o1o1o1o1o1o0l1o0o0o0l1z1o1o2x
        x x x x x o0o0o0o1o1o1o1o1o1o1o2l2o2x
        x x x x x x o1o0o0o0o0z1z2o2o2o2o1o1o1
        x x x x x x o1o1l1o1o1o1o1o1o1o1o1o2o1
        x x x x x x x x o0o0o0o0z1z2o2o2o2o2o1
        x x x x x x x x x o1o1o1o0o1s2o0o0o1o1
        x x x x x x x x x x x o1l1o1o1Z1o0o1x
        x x x x x x x x x x x x o0o0o0z1l1o1x
        x x x x x x x x x x x x x x x x o0o0x
        """
      }

    , { emoji = "🐼" -- 36
      , name = "panda"
      , maze ="""
        sz:20,23;st:5,21;end:14,0;mz:
        x x o3o3z4o4o4o4o4x x x x x x x x x x x
        x x o3o4z5o5o5o5o4x x x x x x x x x x x
        x o3o3o4o4l5o4o5o4z5o5x x x x x x x x x
        o2o3z4z5o5o5o4o5o4o5o5x x x x x x x x x
        o2o3o3o3o4l5o4o5o4o5s5x x x x x x x x x
        o2o2z3o3o4s5o5o5o4o5o4x x x x x x x x x
        s2o2o2o3o4o4o5o4o4s5o4o4x x x x x x x x
        o1o3z4z5o5o5o5o4o3l4o3o4z5o5x x x x x x
        o1o1o3z4o4o4o4o4o3o4o3o4o5o5o5x x x x x
        x x o3o3l4o3o3o3o3o4o3o4l5o4o5o5o5o5x x
        x x o1o1o4o4o4o4Z4l4o3o4o5o4o3z4o4o5x x
        x x x o1o1z2z3o3z4o4o3o4o5o4o3o4o4o4x x
        x x x x o1o1o1s3o3o3o3o4o4o4o3o4o3o4z5o5
        x x x x x x o1o2o2o1z2z3o3o3o3o4o3o4o5o5
        x x x x x x o1o1l2o1o2o2o2o2o2s4o3o4o5s5
        x x x x x x x x o2o2o2Z2o1o1o3o3o3o4o5o4
        x x x x x x x x x o1o1o1o1o2s3o2z3l4o3o4
        x x x x x x x x x o0o0o2o2o2o2o2o4o4o3o4
        x x x x x x x x x x o0z1o1o3o3o3s4o4o3o4
        x x x x x x x x x x x x o1o1s3o3o3o4o3o4
        x x x x x x x x x x x x o1z2o2l3o2o2o3o4
        x x x x x x x x x x x x x x o3o3Z3o2o3o4
        x x x x x x x x x x x x x x o0z1z2z3o3x
        """
      }

    , { emoji = "🦌" -- 37
      , name = "deer"
      , maze ="""
        sz:24,24;st:8,23;end:19,0;mz:
        x x x x x o6z7z8o8x x x x x x x x x x x x x x x
        x x x x o7l7o7o7s8x x x x x x x x x x x x x x x
        x x o6z7o7o6o6o7o7z8o8x x x x x x x x x x x x x
        x o4s6o5o5o5o6o6o6o6s8x x x x x x x x x x x x x
        x o4o5o5o6o5o5o5o5o6l7o6x x x x x x x x x x x x
        x o4o4o4s6o4o4o4o5o5o7o6z7o7x x x x x x x x x x
        o2z3o3o4l5o4o5o4o4o5o7o7o7o7o5o5o5x x x x x x x
        o2o2o3o3o5o5o5o5o4o5z6o6l7o6o5o4l5o4x x x x x x
        x o2o2o3o3o3o3s5o4s5o4o7o7o6o5o4o5o4x x x x x x
        x x o2o2o2o3z4o4o4o4o4o4o4s6s5o4o5o4z5o5x x x x
        x x x x o2o3o3o3o3l4o3o3o3s5o4o4o5o4o5o5o5x x x
        x x x x o2o2o2o2s3o4o4o4l4o4s4o4s5o3o5s5o5x x x
        x x x x x o1o1o2o2o2o2o2o3o3o3z4o4o3o5o4o5o5o5x
        x x x x x x o1o1o1o1o1s2o3o2o2z3o3o3s5o4l5o4o5x
        x x x x x x x x o2o2o1o1s3s2o1o1o3z4o4s4o5o4o5o5
        x x x x x x x x x o2o2o2o2o1o1o2o2o2z3o3o5o4o3o5
        x x x x x x x x x x s2o1o2o1o2o2o1o1o1o3o3s4o3s5
        x x x x x x x x x x o1o1o2o1o2o1o1o2o1o1o3o3o3o4
        x x x x x x x x x x x x o2o1l2o1o2o2s1o1o1o1o1s4
        x x x x x x x x x x x x x o1o2o2o2o1o0o1o0l1o0o3
        x x x x x x x x x x x x x o1o1o1o1o1o0o1o0o1o0s3
        x x x x x x x x x x x x x x x o0o0o0o0o0o0o1o0s2
        x x x x x x x x x x x x x x x x x o1o1o1o1o1o0o1
        x x x x x x x x x x x x x x x x x x x o0o0o0o0x
        """
      }

    , { emoji = "🐄" -- 38
      , name = "cow"
      , maze ="""
        """
      }

    , { emoji = "🐂" -- 39
      , name = "ox"
      , maze ="""
        """
      }

    , { emoji = "🐎" -- 40
      , name = "horse"
      , maze ="""
        """
      }

    , { emoji = "🦓" -- 41
      , name = "zebra"
      , maze ="""
        """
      }

    , { emoji = "🦬" -- 42
      , name = "bison"
      , maze ="""
        """
      }

    -- , { emoji = "🦅" -- 43
    --   , name = "eagle"
    --   , maze ="""
    --     """
    --   }

    -- , { emoji = "🐊" -- 44
    --   , name = "croco"
    --   , maze ="""
    --     """
    --   }

    -- , { emoji = "🐺" -- 45
    --   , name = "wolf"
    --   , maze ="""
    --     """
    --   }

    -- , { emoji = "🐯" -- 46
    --   , name = "tiger"
    --   , maze ="""
    --     """
    --   }

    -- , { emoji = "🦁" -- 47
    --   , name = "lion"
    --   , maze ="""
    --     """
    --   }

    -- , { emoji = "🐻" -- 48
    --   , name = "bear"
    --   , maze ="""
    --     """
    --   }

    -- , { emoji = "🐫" -- 49
    --   , name = "camel"
    --   , maze ="""
    --     """
    --   }

    -- , { emoji = "🦏" -- 50
    --   , name = "rhino"
    --   , maze ="""
    --     """
    --   }

    -- , { emoji = "🦛" -- 51
    --   , name = "hippo"
    --   , maze ="""
    --     """
    --   }

    -- , { emoji = "🦒" -- 52
    --   , name = "giraffe"
    --   , maze ="""
    --     """
    --   }

    -- , { emoji = "🐘" -- 53
    --   , name = "elephant"
    --   , maze ="""
    --     """
    --   }

    -- , { emoji = "🦣" -- 54
    --   , name = "mammoth"
    --   , maze ="""
    --     """
    --   }

    -- , { emoji = "🐬" -- 55
    --   , name = "dolphin"
    --   , maze ="""
    --     """
    --   }

    -- , { emoji = "🐳" -- 56
    --   , name = "whale"
    --   , maze ="""
    --     """
    --   }

    -- , { emoji = "🦈" -- 57
    --   , name = "shark"
    --   , maze ="""
    --     """
    --   }

    -- , { emoji = "🦖" -- 58
    --   , name = "t-rex"
    --   , maze ="""
    --     """
    --   }

    -- , { emoji = "🐉" -- 59
    --   , name = "dragon"
    --   , maze ="""
    --     """
    --   }

    -- , { emoji = "🦄" -- 60
    --   , name = "unicorn"
    --   , maze ="""
    --     """
    --   }
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
