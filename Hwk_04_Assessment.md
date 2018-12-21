### Assessment for Homework 04

Below are the automated scores for Homework 4.  If you feel that our scripts are incorrectly assessing your work then please email ``csci2041@umn.edu`` and explain the problem.  If your code is right you will get credit for it - just maybe not right away.

Please note that the Extlint part does not cost any points in this homework.

#### Total score: _122_ / _129_

Run on April 06, 05:33:35 AM.

+ Pass: Change into directory "Hwk_04".

## Assessment for part 1

+ Pass: Check that file "expr_functions.ml" exists.

+  _3_ / _3_ : Pass: Check that an OCaml file "expr_functions.ml" has no syntax or type errors.

    OCaml file "expr_functions.ml" has no syntax or type errors.



+  _2_ / _2_ : Pass: Check that an OCaml file "expr_functions.ml" has warnings.

    OCaml file "expr_functions.ml" has no warnings.



Please note that whitespace in not considered in the test for ```serialize``` and ```unparse```.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   serialize ( Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3))) )
   ```
   matches the pattern `"Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))"`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   serialize ( Let ("y", Val (Int 5), Let ("x", Add (Id "y", Val (Int 5)), Add (Id "x", Id "y"))) )
   ```
   matches the pattern `"Let (\"y\", Val (Int 5), Let (\"x\", Add (Id \"y\", Val (Int 5)), Add (Id \"x\", Id \"y\")))"`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   serialize ( Lambda ("n", Add (Id "n", Val (Int 1))) )
   ```
   matches the pattern `"Lambda (\"n\", Add (Id \"n\", Val (Int 1)))"`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   unparse ( Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3))) )
   ```
   matches the pattern `"(1 + (2 * 3))"`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   unparse ( Let ("y", Val (Int 5), Let ("x", Add (Id "y", Val (Int 5)), Add (Id "x", Id "y"))) )
   ```
   matches the pattern `"(let y = 5 in (let x = (y + 5) in (x + y)))"`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   unparse ( Lambda ("n", Add (Id "n", Val (Int 1))) )
   ```
   matches the pattern `"(fun n -> (n + 1))"`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   unparse (App (( Lambda ("n", Add (Id "n", Val (Int 1))) ), Val (Int 4)))
   ```
   matches the pattern `"((fun n -> (n + 1)) 4)"`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   freevars ( Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3))) )
   ```
   matches the pattern `[]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   freevars ( Let ("y", Val (Int 5), Let ("x", Add (Id "y", Val (Int 5)), Add (Id "x", Id "y"))) )
   ```
   matches the pattern `[]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   freevars ( Lambda ("n", Add (Id "n", Val (Int 1))) )
   ```
   matches the pattern `[]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   freevars (Id "x")
   ```
   matches the pattern `["x"]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   freevars (Lambda ("x", Id "y"))
   ```
   matches the pattern `["y"]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   freevars (Let ("x", Id "x", Id "y"))
   ```
   matches the pattern `["x"; "y"]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   freevars (LetRec ("x", Id "x", Id "y"))
   ```
   matches the pattern `["y"]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate e1
   ```
   matches the pattern `Int 7`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate e2
   ```
   matches the pattern `Int 7`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate e3
   ```
   matches the pattern `Bool true`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate e4
   ```
   matches the pattern `Bool false`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate e5
   ```
   matches the pattern `Bool true`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate e6
   ```
   matches the pattern `Int 15`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate e7
   ```
   matches the pattern `Bool true`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate (App (inc, Val (Int 4)))
   ```
   matches the pattern `Int 5`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate (Add (Val (Int 2), Val (Int 3)))
   ```
   matches the pattern `Int 5`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate (App (inc', Val (Int 4)))
   ```
   matches the pattern `Int 5`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate add2app
   ```
   matches the pattern `Int 6`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate (App (sumToN, Val (Int 4)))
   ```
   matches the pattern `Int 10`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate (App (sumToN, Val (Int 10)))
   ```
   matches the pattern `Int 55`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate (App (sumToN, Val (Int 100)))
   ```
   matches the pattern `Int 5050`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate (App (fact, Val (Int 0)))
   ```
   matches the pattern `Int 1`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate (App (fact, Val (Int 1)))
   ```
   matches the pattern `Int 1`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate (App (fact, Val (Int 2)))
   ```
   matches the pattern `Int 2`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate (App (fact, Val (Int 4)))
   ```
   matches the pattern `Int 24`.

   




### Tests in addition to feedback tests

+  _0_ / _2_ : Fail: 
Check that the result of evaluating
   ```
   evaluate (Let ("x", Val (Int 1), Lambda ("n", Id "n")))
   ```
   matches the pattern `Closure ("n", Id "n", [])`.

   


   Your solution evaluated incorrectly and produced some part of the following:

    ```
    - : value = Closure ("n", Id "n", [("x", Int 1)])
    ```


+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate (App (
   LetRec ("fib",
           Lambda ("x",                                                                                
                   If (Lt (Id "x", Val (Int 2)), 
                       Id "x",                                               
                       Add (App (Id "fib", Sub (Id "x", Val (Int 1))),                                 
                            App (Id "fib", Sub (Id "x", Val (Int 2)))))),                              
           Id "fib"), Val (Int 0)))
   ```
   matches the pattern `Int 0`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate (App (
   LetRec ("fib",
           Lambda ("x",                                                                                
                   If (Lt (Id "x", Val (Int 2)), 
                       Id "x",                                               
                       Add (App (Id "fib", Sub (Id "x", Val (Int 1))),                                 
                            App (Id "fib", Sub (Id "x", Val (Int 2)))))),                              
           Id "fib"), Val (Int 5)))
   ```
   matches the pattern `Int 5`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate (App (
   LetRec ("fib",
           Lambda ("x",                                                                                
                   If (Lt (Id "x", Val (Int 2)), 
                       Id "x",                                               
                       Add (App (Id "fib", Sub (Id "x", Val (Int 1))),                                 
                            App (Id "fib", Sub (Id "x", Val (Int 2)))))),                              
           Id "fib"), Val (Int 9)))
   ```
   matches the pattern `Int 34`.

   




### Check for requirements in `Writing transparent code`

+  _2_ / _5_ : Fail: Check `unparse` function for requirements in `Writing transparent code`.

    Poor variable naming choice, and one line over 80 chars long.

+  _5_ / _5_ : Pass: Check `eval` function for requirements in `Writing transparent code`.

    

## Assessment for part 2

+ Pass: Check that file "expr_let_typing.ml" exists.

+  _3_ / _3_ : Pass: Check that an OCaml file "expr_let_typing.ml" has no syntax or type errors.

    OCaml file "expr_let_typing.ml" has no syntax or type errors.



+  _2_ / _2_ : Pass: Check that an OCaml file "expr_let_typing.ml" has warnings.

    OCaml file "expr_let_typing.ml" has no warnings.



+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate e1
   ```
   matches the pattern `OK (Int 7)`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate e2
   ```
   matches the pattern `OK (Int 7)`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate e3
   ```
   matches the pattern `OK (Bool true)`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate e4
   ```
   matches the pattern `OK (Bool false)`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate e5
   ```
   matches the pattern `OK (Bool true)`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate e6
   ```
   matches the pattern `OK (Int 15)`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate e7
   ```
   matches the pattern `OK (Bool true)`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate er1
   ```
   matches the pattern `Err [IncorrectType (Val (Bool true), BoolType, [IntType])]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate er2
   ```
   matches the pattern `Err [IncorrectType (Val (Int 3), IntType, [BoolType])]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate er5
   ```
   matches the pattern `Err [UnboundName "y"]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate er6
   ```
   matches the pattern `Err [DivisionByZero (Div (Val (Int 5), Id "y"))]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   evaluate (Add (Val (Bool true), Val (Bool false)))
   ```
   matches the pattern `Err [IncorrectType (Val (Bool true), BoolType, [IntType])]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   check e8 []
   ```
   matches the pattern `OK IntType`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   check er1 []
   ```
   matches the pattern `Err [IncorrectType (Val (Bool true), BoolType, [IntType])]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   check er2 []
   ```
   matches the pattern `Err [IncorrectType (Val (Int 3), IntType, [BoolType])]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   check er5 []
   ```
   matches the pattern `Err [UnboundName "y"]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   check (Add (Val (Bool true), Val (Bool false))) []
   ```
   matches the pattern `Err
[IncorrectType (Val (Bool true), BoolType, [IntType]);
IncorrectType (Val (Bool false), BoolType, [IntType])]`.

   




### Check for requirements in `Writing transparent code`

+  _4_ / _5_ : Pass: Check `eval` function for requirements in `Writing transparent code`.

    

+  _4_ / _5_ : Pass: Check `check` function for requirements in `Writing transparent code`.

    

#### Total score: _122_ / _129_

