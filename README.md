# time-traveling interpreter for degugging

## How to run 
The interpreter can be run by passing the path to the file as an argument - otherwise it defaults to `tti.prg`
```shell
stack exec tti <path to prg>
```
#### GCD Example 
Provided are a working gcd.prg and a wrong version tti.prg. They each try and calculate the greates common devisor between 98 and 56 - which is 14. Run them using:
```shell
stack exec tti
stack exec tti gcd.prg
```

## Commands

* H  - displays help 
* S  - step through program 
* C  - continue programm 
* R  - reverse/ step back to last state change
* L  - List break points
* Break (Bool Expr) - set a breakpoint 
* Clear - clear all breakpoints

## The Flow
At each breakpoint the interpreter show the last statement that updated the state (e.g past) and the next statement it will execute.  
Breakpoints stop the programm if the conditioned specified is True - else it continues.  
When using step will continue by one statemnt - note: step will pause after evaluating an if or while expression before stepping into the boddy.  
Reverse steps all the way back to the last command that modified the state (ignoring statements that did not affect state)  
See example:
```shell
  past >> Assign "b" (Const (I 56))
State: [("a",I 98),("b",I 56)]
 next >> While (Not (Eq (Var "a") (Var "b"))) Pass
> S
 past >> Assign "b" (Const (I 56))
State: [("a",I 98),("b",I 56)]
 next >> If (Gt (Var "a") (Var "b")) Pass Pass
> S
 past >> Assign "b" (Const (I 56))
State: [("a",I 98),("b",I 56)]
 next >> Assign "a" (Sub (Var "a") (Var "b"))
> S
 past >> Assign "a" (Sub (Var "a") (Var "b"))
State: [("a",I 42),("b",I 56)]
 next >> While (Not (Eq (Var "a") (Var "b"))) Pass
> R
 past >> Assign "b" (Const (I 56))
State: [("a",I 98),("b",I 56)]
 next >> Assign "a" (Sub (Var "a") (Var "b"))
> R
 past >> Assign "a" (Const (I 98))
State: [("a",I 98)]
 next >> Assign "b" (Const (I 56))
> _
```
