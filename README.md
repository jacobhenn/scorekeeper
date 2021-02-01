# Description

Scorekeeper is a slick and lightweight command line score tracking program, written in haskell.

help table:

```
╭───────────────────────────────────────────────────────────────────╮
│ scorekeeper v3.3.0 (https://github.com/jacobhenn/scorekeeper)     │
├───────────────────┬───────────────────────────────────────────────┤
│ [player]          │ add 1 point to [player]                       │
│ [n] [player]      │ add [n] (integer) points to [player]          │
│ :add [player] ... │ add [player](s) to the list of players        │
│ :rm [player] ...  │ remove [player](s) from the list of players   │
│ :mul [n]          │ multiply each player's score by [n] (integer) │
│ :q, :quit, :exit  │ exit scorekeeper                              │
│ :h, :help         │ show this message                             │
╰───────────────────┴───────────────────────────────────────────────╯
```


# Use

run/compile/install scorekeeper with [Cabal](https://www.haskell.org/cabal) (remove the `-dynamic` flag in `scorekeeper.cabal` if you usually compile static executables)
