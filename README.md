# Description

Scorekeeper is a slick and lightweight command line score tracking program, written in haskell.

help table:

```
╭───────────────────────────────────────────────────────────────╮
│ scorekeeper v5.0.3 (https://github.com/jacobhenn/scorekeeper) │
├───────────────────────────────────────────────────────────────┤
│ prefix searches match players whose names start with the      │
│ given string: b = bob, ill != bill                            │
├─────────────────┬─────────────────────────────────────────────┤
│ [prefix]        │ add 1 point to the player matching [prefix] │
│ [n] [prefix]    │ add [n] points to [prefix]                  │
│ :add [player].. │ add [player](s) to the list of players      │
│ :rm [prefix]..  │ remove [prefix](s) from the list of players │
│ :mul [n]        │ multiply each player's score by [n]         │
│ :q, :quit       │ exit scorekeeper                            │
│ :h, :help       │ show this message                           │
╰─────────────────┴─────────────────────────────────────────────╯
```

# Use

run/compile/install scorekeeper with [Cabal](https://www.haskell.org/cabal) (remove the `-dynamic` flag in `scorekeeper.cabal` if you usually compile static executables)
