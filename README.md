# advent2022
Solutions to [Advent of code 2022](https://adventofcode.com/2022) in Haskell. How beautiful!

## Building
This project uses [cabal](https://cabal.readthedocs.io/en/3.4/getting-started.html) for builds and dependency management. To build the package run `cabal build`. I have used GHC version 9.2.7 and cabal-install version 3.10.1.0 for this project. A `shell.nix` file is provided to create a local development environment. If you have [nix](https://nixos.org/) installed, you may run `nix-shell` to enter a development environment for this project. 

```
$ nix-shell

[nix-shell:~/stuff/advent]$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.2.7

[nix-shell:~/stuff/advent]$ cabal --version
cabal-install version 3.10.1.0
compiled using version 3.10.1.0 of the Cabal library
```

## Running
There is only one executable in this package that is named `advent`. The executable takes three command-line arguments - day, part, and input type. Day is Advent of Code 2022 Day, "day10", for example. Part should be either "part1" or "part2", and input type should be either "example" or "real". "example" runs the solution against example input for the problem and "real" runs the solution against my personal input for the problem. 

To run a solution on your own input, replace the `inputs/<day>.txt` file's contents with your input.

### Example runs 
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
