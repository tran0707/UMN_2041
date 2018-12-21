## Feedback for Homework 07

Run on April 19, 21:53:30 PM.

## Feedback for Homework 7

+ Pass: Check that directory "Hwk_07" exists.

+ Pass: Change into directory "Hwk_07".

## Feedback for part 1 - graph coloring

+ Pass: Check that file "graph_coloring.ml" exists.

+ Pass: Check that an OCaml file "graph_coloring.ml" has no syntax or type errors.

    OCaml file "graph_coloring.ml" has no syntax or type errors.



The functions ``test_color_option`` and ``test_color_exception`` take a graph as input test that your function (either ``color_option`` or ``color_exception``) produce a correct result.

+ Pass: 
Check that the result of evaluating
   ```
   test_color_option (( [N 1; N 2; N 3; N 4], [ (N 1,N 2); (N 1,N 3); (N 2,N 3); (N 3,N 4) ] )) 
   ```
   matches the pattern `"OK"`.

   




+ Pass: 
Check that the result of evaluating
   ```
   test_color_exception (( [N 1; N 2; N 3; N 4], [ (N 1,N 2); (N 1,N 3); (N 2,N 3); (N 3,N 4) ] )) 
   ```
   matches the pattern `"OK"`.

   




## Feedback for part 2 - the water jug problem

+ Pass: Check that file "water_jug.ml" exists.

+ Pass: Check that an OCaml file "water_jug.ml" has no syntax or type errors.

    OCaml file "water_jug.ml" has no syntax or type errors.



The function ``test_waterjug`` checks that your ``play``function produces a correct sequence of operations.

+ Pass: 
Check that the result of evaluating
   ```
   test_waterjug (play ())
   ```
   matches the pattern `"OK"`.

   




