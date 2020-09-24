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
[scorekeeper] save test
------------------------
0 bill
0 dave
5 joe
[scorekeeper] ls
test
------------------------
0 bill
0 dave
5 joe
[scorekeeper] add florb
------------------------
0 bill
0 dave
0 florb
5 joe
[scorekeeper] load test
------------------------
0 bill
0 dave
5 joe
[scorekeeper] quit
```
(saving and loading functionality is temporarily unavailable due to rewrite)

# Use

run/compile/install scorekeeper with [Cabal](https://www.haskell.org/cabal)
