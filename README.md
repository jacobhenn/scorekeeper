# Description

Scorekeeper is a very lightweight command line score tracking program, written in haskell.
Example usage:
```
$ scorekeeper
[initialize teams] bill joe bob
------------------------
0 bill
0 joe
0 bob
[score, name] 5 joe
------------------------
0 bill
5 joe
0 bob
[score, name] bob
------------------------
0 bill
5 joe
1 bob
[score, name] -2 joe
------------------------
0 bill
3 joe
1 bob
[score, name] exit
```

# Compile

`ghc --make scorekeeper`
