# splank: STRIPS-like planner

Uses [minisat](http://minisat.se) through [satplus](https://github.com/koengit/satplus).

## Example

Input:

```prolog
domain Loc a,b,c
domain Level low,high
pred At(Loc), Level(Level), BoxAt(Loc), BananasAt(Loc), HaveBananas

initial At(a), Level(low), BoxAt(c), BananasAt(b)
goal HaveBananas

action Move(X,Y)
  from At(X), Level(low)
  to   not At(X), At(Y)

action ClimbUp(L)
  from At(L), BoxAt(L), Level(low)
  to   Level(high), not Level(low)

action ClimbDown(L)
  from At(L), BoxAt(L), Level(high)
  to   Level(low), not Level(high)

action MoveBox(X,Y)
  from At(X), BoxAt(X), Level(low)
  to   BoxAt(Y), not BoxAt(X), At(Y), not At(X)

action TakeBananas(L)
  from At(L), BananasAt(L), Level(high)
  to   HaveBananas
```

Output:

```prolog
--> Adding action 0
    (2 choices)
--> Adding action 1
    (7 choices)
--> Adding action 2
    (19 choices)
--> Adding action 3
    (19 choices)
=> Solution
Move(a,c)
MoveBox(c,b)
ClimbUp(b)
TakeBananas(b)
```
