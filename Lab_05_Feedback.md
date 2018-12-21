### Feedback for Lab 05

Run on April 21, 05:33:33 AM.

+ Pass: Change into directory "Lab_05".

+ Pass: Check that file "higher.ml" exists.

+ Pass: Check that an OCaml file "higher.ml" has no syntax or type errors.

    OCaml file "higher.ml" has no syntax or type errors.



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

   




+ Pass: You should remove unnecessary ;;

+ Pass: Check that file "puzzle.ml" exists.

+ Pass: Check that an OCaml file "puzzle.ml" has no syntax or type errors.

    OCaml file "puzzle.ml" has no syntax or type errors.



+ Pass: Make sure you are only using recursion in functions read_file, read_chars

   



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

   




+ Pass: You should remove unnecessary ;;

+ Pass: Check that file "formatter.ml" exists.

+ Pass: Check that an OCaml file "formatter.ml" has no syntax or type errors.

    OCaml file "formatter.ml" has no syntax or type errors.



+ Pass: Make sure you are only using recursion in functions read_file, read_chars, explode, f

   



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

   




+ Pass: You should remove unnecessary ;;

