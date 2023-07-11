# advent2022
Solutions to [Advent of code 2022](https://adventofcode.com/2022) in Haskell. How beautiful!

# Building and running
This project uses cabal for builds and dependency management. To build the package run `cabal build`. 

There is only one executable in this package that is named `advent`. The executable takes three command-line arguments - day, part, and input type. Day is Advent of Code 2022 Day, "day10", for example. Part should be either "part1" or "part2", and input type should be either "example" or "real". "example" runs the solution against example input for the problem and "real" runs the solution against my personal input for the problem. 

To run a solution on your own input, replace the `inputs/<day>.txt` file's contents with your input.

## Example runs 
```
$ cabal run advent -- day10 part2 real
###..####.####.#..#.####.####.#..#..##..
#..#....#.#....#.#..#....#....#..#.#..#.
#..#...#..###..##...###..###..####.#..#.
###...#...#....#.#..#....#....#..#.####.
#.#..#....#....#.#..#....#....#..#.#..#.
#..#.####.####.#..#.####.#....#..#.#..#.

$ cabal run advent -- day10 part2 example
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....

$ cabal run advent -- day10 part1 real
14820

$ cabal run advent -- day5 part1 real
JRVNHHCSJ

$ cabal run advent -- day5 part1 example
CMZ
```
