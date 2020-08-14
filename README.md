# Description

Scorekeeper is a very lightweight command line score tracking program, written in haskell.
Example usage:
```
$ scorekeeper bill bob joe
------------------------
0 bill
0 bob
0 joe
[scorekeeper] add frank dave
------------------------
0 bill
0 bob
0 joe
0 frank
0 dave
[scorekeeper] bob
------------------------
0 bill
0 joe
0 frank
0 dave
1 bob
[scorekeeper] 5 joe
------------------------
0 bill
0 frank
0 dave
1 bob
5 joe
[scorekeeper] del frank bob
------------------------
0 bill
0 dave
5 joe
[scorekeeper] quit
```

# Compile

`ghc --make scorekeeper`

# Known Issues

Entering a two-word command where the first word is nonsense and the second word is one of the team names causes the exception `Prelude.read: no parse`. The cause of this is known and it will be fixed.
