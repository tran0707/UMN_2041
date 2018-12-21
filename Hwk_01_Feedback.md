## Feedback for Homework 01

Run on February 11, 16:37:07 PM.

+ Pass: Change into directory "Hwk_01".

### Feedback for Part 1

+ Pass: Check that file "intro.ml" exists.

+ Pass: Check that an OCaml file "intro.ml" has no syntax or type errors.

    OCaml file "intro.ml" has no syntax or type errors.



+ Pass: 
Check that the result of evaluating
   ```
   even 4
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   even 5
   ```
   matches the pattern `false`.

   




+ Pass: 
Check that the result of evaluating
   ```
   euclid 6 9
   ```
   matches the pattern `3`.

   




+ Pass: 
Check that the result of evaluating
   ```
   euclid 5 9
   ```
   matches the pattern `1`.

   




+ Pass: 
Check that the result of evaluating
   ```
   frac_simplify (8,16)
   ```
   matches the pattern `(1,2)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   frac_simplify (4,9)
   ```
   matches the pattern `(4,9)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   frac_simplify (3,9)
   ```
   matches the pattern `(1,3)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   take 3 [1; 2; 3; 4; 5]
   ```
   matches the pattern `[1; 2; 3]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   take 0 ['a'; 'b'; 'c']
   ```
   matches the pattern `[]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   take (-2) ['a'; 'b'; 'c']
   ```
   matches the pattern `[]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   max [1; 2; 5; 3; 2]
   ```
   matches the pattern `5`.

   




+ Pass: 
Check that the result of evaluating
   ```
   max [-1; -2; -5; -3; -2]
   ```
   matches the pattern `-1`.

   




### Feedback for Part 2

+ Pass: Check that file "higher.ml" exists.

+ Pass: Check that an OCaml file "higher.ml" has no syntax or type errors.

    OCaml file "higher.ml" has no syntax or type errors.



+ Pass: You are not allowed to use recursion.

   



+ Pass: 
Check that the result of evaluating
   ```
   all_evens [1; 2; 3; 4; 5]
   ```
   matches the pattern `[2; 4]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   all_evens [1; 3; 5]
   ```
   matches the pattern `[]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   increment_all [1; 2; 3; 4; 5]
   ```
   matches the pattern `[2; 3; 4; 5; 6]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   increment_all []
   ```
   matches the pattern `[]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   max_fold [1; 2; 5; 3; 2]
   ```
   matches the pattern `5`.

   




+ Pass: 
Check that the result of evaluating
   ```
   max_fold [-1; -2; -5; -3; -2]
   ```
   matches the pattern `-1`.

   




+ Pass: 
Check that the result of evaluating
   ```
   sum_prod [1; 2; 3]
   ```
   matches the pattern `(6, 6)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   sum_prod [1; 2; 3; 4]
   ```
   matches the pattern `(10, 24)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   sum_prod []
   ```
   matches the pattern `(0, 1)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   split (fun x -> x mod 3 = 0) [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
   ```
   matches the pattern `[[1; 2]; [4; 5]; [7; 8]; [10]]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   split (fun x -> x mod 3 = 0) [1; 2; 3; 3; 4; 5; 6; 6; 6; 7; 8; 9 ;10]
   ```
   matches the pattern `[[1; 2]; []; [4; 5]; []; []; [7; 8]; [10]]`.

   




### Feedback for Part 3

+ Pass: Check that file "puzzle.ml" exists.

+ Pass: Check that an OCaml file "puzzle.ml" has no syntax or type errors.

    OCaml file "puzzle.ml" has no syntax or type errors.



+ Pass: Make sure you are only using recursion in functions read_file, read_chars

   



Note that if your output is in reverse order, it's technically still a correct answer. But to make grading simple, please make sure to match the pattern here.

+ Pass: 
Check that the result of evaluating
   ```
   answers "/class/grades/Spring-2018/csci2041/public-class-repo/Homework/Files/words-small.txt"
   ```
   matches the pattern `["planet"; "smooth"; "change"; "twenty"; "knight"; "street"; "prefix"; "abrade"; "rawest"; "wonton"; "scrawl"; "misled"; "presto"; "upland"; "safari"]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   pretty_answers (answers "/class/grades/Spring-2018/csci2041/public-class-repo/Homework/Files/words-small.txt")
   ```
   matches the pattern `[("lane", "planet"); ("moot", "smooth"); ("hang", "change"); ("went", "twenty"); ("nigh", "knight"); ("tree", "street"); ("refi", "prefix"); ("brad", "abrade"); ("awes", "rawest"); ("onto", "wonton"); ("craw", "scrawl"); ("isle", "misled"); ("rest", "presto"); ("plan", "upland"); ("afar", "safari")]`.

   




### Feedback for Part 4

+ Pass: Check that file "formatter.ml" exists.

+ Pass: Check that an OCaml file "formatter.ml" has no syntax or type errors.

    OCaml file "formatter.ml" has no syntax or type errors.



+ Pass: Make sure you are only using recursion in functions read_file, read_chars, explode, f

   



Note that you might not see the line breaks in the following patterns, because of the way feedback file is rendered. They are in the actual string.

+ Pass: 
Check that the result of evaluating
   ```
   format p1 12
   ```
   matches the pattern `"Hello world!
How are you
today? I
hope all is
well."`.

   




+ Pass: 
Check that the result of evaluating
   ```
   format p1 11
   ```
   matches the pattern `"Hello
world! How
are you
today? I
hope all is
well."`.

   




+ Pass: 
Check that the result of evaluating
   ```
   format p1 20
   ```
   matches the pattern `"Hello world! How are
you today? I hope
all is well."`.

   




+ Pass: 
Check that the result of evaluating
   ```
   format p1 4
   ```
   matches the pattern `"Hello
world!
How
are
you
today?
I
hope
all
is
well."`.

   




