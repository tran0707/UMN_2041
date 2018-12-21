# Hwk_05 Assessment

Run on April 11, 02:18:20 AM.

*Test results are for the code in this repository as it appeared on April 05 at 17:15 CT.*

**Change log:**
+ Wednesday, April 4: (Sean) Minor test adjustments.
+ Saturday, March 31: (Nathan) Extra credit tests.
+ Friday, March 30: (Sean) Initial version.


## Introduction

+  _2_ / _2_ : Pass: Change into directory "Hwk_05".

## Part 1: Evaluating expressions by hand

We will assess this part manually.

+  _1_ / _1_ : Pass: Check that file "question_1.txt" exists.

+  _1_ / _1_ : Pass: Check that file "question_2.txt" exists.

## Part 2: Efficiently computing the conjunction of a list of Boolean values

We will also assess this part manually.

+  _1_ / _1_ : Pass: Check that file "lazy_and.ml" exists.

+ Pass: Check that an OCaml file "lazy_and.ml" has no syntax or type errors.

    OCaml file "lazy_and.ml" has no syntax or type errors.



+ Pass: Check that an OCaml file "lazy_and.ml" has warnings.

    OCaml file "lazy_and.ml" has no warnings.



+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   ands [true; true; true]
   ```
   matches the pattern `true`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   ands []
   ```
   matches the pattern `true`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   ands [true; false; true]
   ```
   matches the pattern `false`.

   




## Part 3: Implementing Streams in OCaml

+  _1_ / _1_ : Pass: Check that file "streams.ml" exists.

+ Pass: Check that an OCaml file "streams.ml" has no syntax or type errors.

    OCaml file "streams.ml" has no syntax or type errors.



+ Pass: Check that an OCaml file "streams.ml" has warnings.

    OCaml file "streams.ml" has no warnings.



### Check: Did you copy `lazy.ml` into your file?

+ Pass: 
Check that the result of evaluating
   ```
   force (ref (Value ()))
   ```
   matches the pattern `()`.

   




+ Pass: 
Check that the result of evaluating
   ```
   demand (ref (Value ()))
   ```
   matches the pattern `()`.

   




+ Pass: 
Check that the result of evaluating
   ```
   demand (delay (fun () -> ()))
   ```
   matches the pattern `()`.

   




### 3.1-3.7

The following tests use helpers from `lazy.ml`.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   take 3 (cubes_from_zip 5)
   ```
   matches the pattern `[125; 216; 343]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   take 2 (drop 3 (from 2018))
   ```
   matches the pattern `[2021; 2022]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   take 3 (cubes_from_map 5)
   ```
   matches the pattern `[125; 216; 343]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   take 3 (cubes_from 5)
   ```
   matches the pattern `[125; 216; 343]`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   take 10 (sieve (from 10))
   ```
   matches the pattern `[10; 11; 12; 13; 14; 15; 16; 17; 18; 19]`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   take 10 (sieve (from 2))
   ```
   matches the pattern `[2; 3; 5; 7; 11; 13; 17; 19; 23; 29]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   sum_positive_prefix ns
   ```
   matches the pattern `7020`.

   




## Extra Credit

If these tests pass, we will assess your work manually.

+ Pass: Check that file "samefringe.ml" exists.

+ Pass: Check that an OCaml file "samefringe.ml" has no syntax or type errors.

    OCaml file "samefringe.ml" has no syntax or type errors.



+ Pass: Check that an OCaml file "samefringe.ml" has warnings.

    OCaml file "samefringe.ml" has no warnings.



+ Pass: 
Check that the result of evaluating
   ```
   eqleaves_lazy t1 t2
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   eqleaves_lazy t1 t3
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   eqleaves_lazy t1 t4
   ```
   matches the pattern `false`.

   




+ Pass: 
Check that the result of evaluating
   ```
   eqleaves_lazy t1 t5
   ```
   matches the pattern `false`.

   




+ Pass: 
Check that the result of evaluating
   ```
   eqleaves_lazy t2 t3
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   eqleaves_lazy t2 t4
   ```
   matches the pattern `false`.

   




+ Pass: 
Check that the result of evaluating
   ```
   eqleaves_lazy t2 t5
   ```
   matches the pattern `false`.

   




+ Pass: 
Check that the result of evaluating
   ```
   eqleaves_lazy t3 t4
   ```
   matches the pattern `false`.

   




+ Pass: 
Check that the result of evaluating
   ```
   eqleaves_lazy t3 t5
   ```
   matches the pattern `false`.

   




+ Pass: 
Check that the result of evaluating
   ```
   eqleaves_lazy t4 t5
   ```
   matches the pattern `true`.

   




## Extlint

Various code style and organization checks are run on your code to detect common errors.

A description of the checks can be found here:  https://github.umn.edu/umn-csci-2041-S18/public-class-repo/blob/master/Course%20Info/extlint.md

### `streams.ml`

+ Pass: Check that file "streams.ml" exists.

+ Pass: **All extlint tests passed!**

### `samefringe.ml`

+ Pass: Check that file "samefringe.ml" exists.

+ Pass: **All extlint tests passed!**

### `lazy_and.ml`

+ Pass: Check that file "lazy_and.ml" exists.

+ Pass: 

```
  --> lazy_and.ml:7:15
   | 
 6 |     | x :: xs ->
 7 |             if (x = false) 
   |                ^^^^^^^^^^^ equality with false
 8 |             then false
   | Instead of `(x = false)`, try writing `not (x)`.
```

You would've lost 1 point if this were an assessment.


## Total for Automated Tests

#### Total score: _24_ / _24_

---

