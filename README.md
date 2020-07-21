# Description

Scorekeeper is a very lightweight command line score tracking program, written in haskell.
Example usage:
```
$ scorekeeper
[initial teams] bill joe bob
------------------------
0 bill
0 joe
0 bob
[score, name] 5 joe
------------------------
0 bill
0 bob
5 joe
[score, name] bob
------------------------
0 bill
1 bob
5 joe
[score, name] -2 joe
------------------------
0 bill
1 bob
3 joe
[score, name] exit
```

# Compile

`ghc --make scorekeeper`
