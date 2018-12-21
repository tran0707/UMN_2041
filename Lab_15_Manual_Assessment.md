+ Pass: Check that directory "Lab_15" exists.

+ Pass: Change into directory "Lab_15".

+ Pass: Check that file "src/translate.ml" exists.

## Map

+ Pass: Check that the result of executing build.sh on `examples/three.src.txt` matches the pattern `Returned \[2, 4, 6, 8\]`.

   



+  _5_ / _5_ : Pass: Check that the result of executing build.sh on `examples/four.src.txt` matches the pattern `Returned \[2, 4, 6, 8\]`.

   



+  _0_ / _10_ : Fail: Check that the result of executing build.sh on `examples/five.src.txt` matches the pattern `Returned 16`.

   

   Your solution evaluated incorrectly and produced some part of the following:

 
   ```
[1;36mBuilding OCaml code...[0m
00:00:00 1    (1   ) src/compiler.ml.depends                       * O------- |[KFinished, 52 targets (52 cached) in 00:00:00.[K
[1;36mCompiling...[0m
[1;34mfunction add(x : int, y : int) =
	(x + y);
function sum_array(arr : int[]) =
	fold(add, arr);
function process(arr : int[]) =
	sum_array(arr);
process([1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])[0m
Raised at file "pervasives.ml", line 30, characters 22-33
[1;31mTODO translate_fold[0m

   ```



## Extlint

Various code style and organization checks are run on your code to detect common errors.

A description of the checks can be found here:  https://github.umn.edu/umn-csci-2041-S18/public-class-repo/blob/master/Course%20Info/extlint.md

### `src/translate.ml`

+ Pass: Check that file "src/translate.ml" exists.

+ Pass: **All extlint tests passed!**

