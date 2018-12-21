### Assessment for Homework 07

#### Total score: _73_ / _75_

Run on April 26, 12:06:52 PM.

## Assessment for Homework 7

+ Pass: Check that directory "Hwk_07" exists.

+ Pass: Change into directory "Hwk_07".

## Assessment for part 1 - graph coloring

+ Pass: Check that file "graph_coloring.ml" exists.

+  _3_ / _3_ : Pass: Check that an OCaml file "graph_coloring.ml" has no syntax or type errors.

    OCaml file "graph_coloring.ml" has no syntax or type errors.



+  _2_ / _2_ : Pass: Check that an OCaml file "graph_coloring.ml" has warnings.

    OCaml file "graph_coloring.ml" has no warnings.



The functions ``test_color_option`` and ``test_color_exception`` take a graph as input test that your function (either ``color_option`` or ``color_exception``) produce a correct result.

+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   test_color_option (( [N 1; N 2; N 3; N 4], [ (N 1,N 2); (N 1,N 3); (N 2,N 3); (N 3,N 4) ] )) 
   ```
   matches the pattern `"OK"`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   test_color_exception (( [N 1; N 2; N 3; N 4], [ (N 1,N 2); (N 1,N 3); (N 2,N 3); (N 3,N 4) ] )) 
   ```
   matches the pattern `"OK"`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   test_color_option (( [N 1; N 2; N 3; N 4], [ (N 1,N 2); (N 1,N 3); (N 1,N 4); (N 2,N 3); (N 2,N 4); (N 3,N 4) ] )) 
   ```
   matches the pattern `"No coloring computed."`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   test_color_exception (( [N 1; N 2; N 3; N 4], [ (N 1,N 2); (N 1,N 3); (N 1,N 4); (N 2,N 3); (N 2,N 4); (N 3,N 4) ] )) 
   ```
   matches the pattern `"No coloring computed."`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   test_color_option (( [N 1; N 2; N 3; N 4; N 5], [ (N 1,N 2); (N 1,N 3); (N 2,N 5); (N 3,N 4); (N 4,N 5) ] )) 
   ```
   matches the pattern `"OK"`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   test_color_exception (( [N 1; N 2; N 3; N 4; N 5], [ (N 1,N 2); (N 1,N 3); (N 2,N 5); (N 3,N 4); (N 4,N 5) ] )) 
   ```
   matches the pattern `"OK"`.

   




### Check for explanation about search strategy

+  _5_ / _5_ : Pass: Check comments about search space.

    

+  _5_ / _5_ : Pass: Check comments about decisions at each step.

    

+  _5_ / _5_ : Pass: Check comments about avoding inefficiency.

    

### Check for requirements in `Writing transparent code`

+  _0_ / _1_ : Fail: Check that file graph_coloring.ml does not contain lines of code over 80 characters wide.

+  _2_ / _2_ : Pass: Check for proper indentation.

    

+  _1_ / _1_ : Pass: Check for type annotation.

    

## Assessment for part 2 - the water jug problem

+ Pass: Check that file "water_jug.ml" exists.

+  _3_ / _3_ : Pass: Check that an OCaml file "water_jug.ml" has no syntax or type errors.

    OCaml file "water_jug.ml" has no syntax or type errors.



+  _2_ / _2_ : Pass: Check that an OCaml file "water_jug.ml" has warnings.

    OCaml file "water_jug.ml" has no warnings.



The function ``test_waterjug`` checks that your ``play``function produces a correct sequence of operations.

+  _7_ / _7_ : Pass: 
Check that the result of evaluating
   ```
   test_waterjug (play ())
   ```
   matches the pattern `"OK"`.

   




+  _5_ / _5_ : Pass: Check comments about state representation.

    

### Check for requirements in `Writing transparent code`

+  _0_ / _1_ : Fail: Check that file water_jug.ml does not contain lines of code over 80 characters wide.

+  _2_ / _2_ : Pass: Check for proper indentation.

    

+  _1_ / _1_ : Pass: Check for type annotation.

    

#### Total score: _73_ / _75_

