---
name: maze-creator
description: Create well-formed and interesting mazes
---

# Maze Creator

You will receive the desired maze dimensions. Here's a good starting point for a 10x10 maze:

    sz:10,10;st:0,0;end:9,9;mz:
    x x x x x x x x x x
    x x x x x x x x x x
    x x x x x x x x x x
    x x x x x x x x x x
    x x x x x x x x x x
    x x x x x x x x x x
    x x x x x x x x x x
    x x x x x x x x x x
    x x x x x x x x x x
    x x x x x x x x x x

The view is from the lower left, the `0,0` point is the lower left, while the `9,9` point is the upper right.

The possible blocks are:
- `x ` nothing, a hole: the space is optional, in fact all spaces are optional
- `o0` basic block, where the second character is the height
- `l1` a bridge: a block passable both at level n and n-1 (the two aren't connected)
- `s1` stairs sloping down in the SE direction, connecting NW `o1` and SE `o0`
- `z1` stairs sloping down in the SW direction, connecting NE `o1` and SW `o0`
- `S1` stairs sloping down in the NW direction, connecting SE `o1` and NW `o0`
- `Z1` stairs sloping down in the NE direction, connecting SW `o1` and NE `o0`

Here's a sample maze, creating sort of a figure eight shape with a bridge in the middle:

    sz:3,5;st:1,0;end:1,4;mz:
    o1o1x
    o1s1x
    o1l1o1
    x S1o1
    x o1o1

## Verification

Run this to verify your maze:

    npm run analyze "sz:3,5;st:1,0;end:1,4;mz:o1o1xo1s1xo1l1o1xS1o1xo1o1"

Output:

    Emoji,Name,Reachable,Occluding,Unreachable,Hanging,Total Cells,Shortest Path,Sol. Density,Stairs,Bridges,River Factor,Loop Count,Greenery,Holes,Squares
    📥,"Custom",1,0,0,0,12,4,0.33,0.17,0.08,0.0,1,0,0,0

Reachable should be `1`. Occluding, Unreachable, Hanging, Greenery, Holes, Squares are all better as `0`, though Holes and Greenery aren't too big problems. Sol. Density is generally good to keep between 0.3 and 0.5, though not strictly necessary.
