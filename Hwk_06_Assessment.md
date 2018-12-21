### Assessment for Homework 06

#### Total score: _109_ / _110_

Run on April 19, 17:35:49 PM.

+ Pass: Check that directory "Hwk_06" exists.

+ Pass: Change into directory "Hwk_06".

+ Pass: Check that file "lazeeModules.ml" exists.

+  _10_ / _10_ : Pass: Check that an OCaml file "lazeeModules.ml" has no syntax or type errors.

    OCaml file "lazeeModules.ml" has no syntax or type errors.



+  _1_ / _1_ : Pass: Check that file lazeeModules.ml does not contain lines of code over 80 characters wide.

+  _2_ / _2_ : Pass: 

Are all helper functions contained within the module definition
they are required for?

    

+  _2_ / _2_ : Pass: 

Does code indentation follow guidelines?

    

+ Pass: Check that file "streamModules.ml" exists.

+  _10_ / _10_ : Pass: 
Check that the result of evaluating
   ```
   true
   ```
   matches the pattern `true`.

   Note: This test is really to check that streamModules can be loaded without errors.




+  _5_ / _5_ : Pass: Check that the Stream signature does not expose unnecessary functions.

    

+  _1_ / _1_ : Pass: Check that file streamModules.ml does not contain lines of code over 80 characters wide.

+  _2_ / _2_ : Pass: 

Are all helper functions contained within the module definition
they are required for?

    

+  _2_ / _2_ : Pass: 

Does code indentation follow guidelines?

    

+ Pass: Check that file "hwk5Modules.ml" exists.

+  _10_ / _10_ : Pass: 
Check that the result of evaluating
   ```
   true
   ```
   matches the pattern `true`.

   Note: This test is really to check that hwk5Modules can be loaded without errors.




+  _0_ / _1_ : Fail: Check that file hwk5Modules.ml does not contain lines of code over 80 characters wide.

+  _2_ / _2_ : Pass: 

Are all helper functions contained within the module definition
they are required for?

    

+  _2_ / _2_ : Pass: 

Does code indentation follow guidelines?

    

+ Pass: Check that file "hwk6.ml" exists.

+ Pass: Ensure that contents of file "hwk6test_base.ml" match what was provided.

+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Lazy.take 4 (Hwk5_Lazy.nats)
   ```
   matches the pattern `[1; 2; 3; 4]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Lazy.take 10 (Hwk5_Lazy.nats)
   ```
   matches the pattern `[1; 2; 3; 4; 5; 6; 7; 8; 9; 10]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Lazy.take 4 (Hwk5_Lazy.cubes_from 1)
   ```
   matches the pattern `[1; 8; 27; 64]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Lazy.take 4 (Hwk5_Lazy.cubes_from_zip 1)
   ```
   matches the pattern `[1; 8; 27; 64]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Lazy.take 4 (Hwk5_Lazy.cubes_from_map 1)
   ```
   matches the pattern `[1; 8; 27; 64]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Lazy.take 2 (Hwk5_Lazy.drop 3 (Hwk5_Lazy.cubes_from 3 ) )
   ```
   matches the pattern `[ 216; 343 ]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Lazy.head (Hwk5_Lazy.drop_until (fun v -> v > 35) Hwk5_Lazy.nats)
   ```
   matches the pattern `36`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Lazy.sum_positive_prefix (Hwk5_Lazy.zip ( - ) (Hwk5_Lazy.from 1000) (Hwk5_Lazy.cubes_from 1))
   ```
   matches the pattern `7020`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Lazy.take 10 Hwk5_Lazy.primes
   ```
   matches the pattern `[2; 3; 5; 7; 11; 13; 17; 19; 23; 29]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Slow.take 4 (Hwk5_Slow.nats)
   ```
   matches the pattern `[1; 2; 3; 4]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Slow.take 10 (Hwk5_Slow.nats)
   ```
   matches the pattern `[1; 2; 3; 4; 5; 6; 7; 8; 9; 10]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Slow.take 4 (Hwk5_Slow.cubes_from 1)
   ```
   matches the pattern `[1; 8; 27; 64]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Slow.take 4 (Hwk5_Slow.cubes_from_zip 1)
   ```
   matches the pattern `[1; 8; 27; 64]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Slow.take 4 (Hwk5_Slow.cubes_from_map 1)
   ```
   matches the pattern `[1; 8; 27; 64]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Slow.take 2 (Hwk5_Slow.drop 3 (Hwk5_Slow.cubes_from 3 ) )
   ```
   matches the pattern `[ 216; 343 ]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Slow.head (Hwk5_Slow.drop_until (fun v -> v > 35) Hwk5_Slow.nats)
   ```
   matches the pattern `36`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Slow.sum_positive_prefix (Hwk5_Slow.zip ( - ) (Hwk5_Slow.from 1000) (Hwk5_Slow.cubes_from 1))
   ```
   matches the pattern `7020`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Slow.take 10 Hwk5_Slow.primes
   ```
   matches the pattern `[2; 3; 5; 7; 11; 13; 17; 19; 23; 29]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Lazy.take 2 (Hwk5_Lazy.zip (fun x y -> x ^ (string_of_int y)) (repeats_lazy "number: ") Hwk5_Lazy.primes)
   ```
   matches the pattern `["number: 2"; "number: 3"]`.

   




+  _3_ / _3_ : Pass: 
Check that the result of evaluating
   ```
   Hwk5_Slow.take 2 (Hwk5_Slow.zip (fun x y -> x ^ (string_of_int y)) (repeats_slow "number: ") Hwk5_Slow.primes)
   ```
   matches the pattern `["number: 2"; "number: 3"]`.

   




#### Total score: _109_ / _110_

