# Hwk_08 Assessment

Run on May 06, 05:04:44 AM.

*Test results are for the code in this repository as it appeared on May 02 at 00:30 CT.*

**Change log:**
+ Wednesday, May 2: (Sean) Initial version.


## Getting started

+ Pass: Check that directory "Hwk_08" exists.

+ Pass: Change into directory "Hwk_08".

+ Pass: Check that file "ordered.ml" exists.

+ Pass: Check that the command ``rm -f ordered.byte binomialheap.byte redblacktree.byte`` executes with return code of 0.

    The command succeeded.

+ Pass: Check that the command ``ocamlbuild ordered.byte`` executes with return code of 0.

    The command succeeded.


## Implement red-black trees
**Note:** These tests may fail if your implementation exhibits reasonably
avoidable issues such as syntax errors, warnings, unbound values, missing
signature constraints, and missing sharing constraints.

For this reason, the point value for these items is low, and most of the points
for this assignment are in the manual assessment. From there you can receive
partial credit for your attempt.

These first few tests are identical to the feedback.

+  _1_ / _1_ : Pass: Check that file "redblacktree.ml" exists.

+  _1_ / _1_ : Pass: Check that the command ``ocamlbuild redblacktree.byte`` executes with return code of 0.

    The command succeeded.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   RBTI.member 8 (List.fold_left (fun t e -> RBTI.insert e t)
                              RBTI.E
                              [1;2;3;4;5])
   ```
   matches the pattern `false`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   RBTI.member 3 (List.fold_left (fun t e -> RBTI.insert e t)
                              RBTI.E
                              [1;2;3;4;5])
   ```
   matches the pattern `true`.

   




### Let's try some more.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   RBTI.member 1 RBTI.E
   ```
   matches the pattern `false`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   RBTI.insert 2 (RBTI.insert 1 RBTI.E)
   ```
   matches the pattern `RBTI.T (RBTI.B, RBTI.E, 1, RBTI.T (RBTI.R, RBTI.E, 2, RBTI.E))`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   RBTI.insert 1 RBTI.E
   ```
   matches the pattern `RBTI.T (RBTI.B, RBTI.E, 1, RBTI.E)`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   RBTI.member 2 (RBTI.T (RBTI.B,
                       RBTI.E,
                       1,
                       RBTI.T (RBTI.R, RBTI.E, 2, RBTI.E)))
   ```
   matches the pattern `true`.

   




## Verify binomial heaps
**Note:** These tests may fail if your implementation exhibits reasonably
avoidable issues such as syntax errors, warnings, unbound values, missing
signature constraints, and missing sharing constraints.

For this reason, the point value for these items is low, and most of the points
for this assignment are in the manual assessment. From there you can receive
partial credit for your attempt.

We'll start again with the feedback tests.

+  _1_ / _1_ : Pass: Check that file "binomialheap.ml" exists.

+  _1_ / _1_ : Pass: Check that the command ``ocamlbuild binomialheap.byte`` executes with return code of 0.

    The command succeeded.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   BHI.isBinomialTree (BHI.Node (2, 5, [BHI.Node (0, 1, [])]))
   ```
   matches the pattern `false`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   BHI.isBinomialTree (BHI.Node (1, 5, [BHI.Node (0, 10, [])]))
   ```
   matches the pattern `true`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   BHI.isBinomialTree (BHI.Node (0, 10, []))
   ```
   matches the pattern `true`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   BHI.isBinomialTree (BHI.Node (2, 5, [BHI.Node (0, 10, [])]))
   ```
   matches the pattern `false`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   BHI.isBinomialHeap [BHI.Node (0, 10, []);
                    BHI.Node (1, 5, [BHI.Node (0, 15, [])])]
   ```
   matches the pattern `true`.

   




### Let's keep going.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   BHI.isBinomialTree (BHI.Node (1, 10, [BHI.Node (0, 5, [])]))
   ```
   matches the pattern `false`.

   




+  _0_ / _1_ : Fail: 
Check that the result of evaluating
   ```
   BHI.isBinomialHeap [BHI.Node (0, 10, []);
                    BHI.Node (0, 5, [])]
   ```
   matches the pattern `false`.

   


   Your solution evaluated incorrectly and produced some part of the following:

    ```
      
    - : bool = true
    ```


+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   BHI.isBinomialTree (BHI.Node (1, 10, []))
   ```
   matches the pattern `false`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   BHI.isBinomialTree (BHI.Node (2,
                              5,
                              [BHI.Node (1, 10, [BHI.Node (0, 20, [])]);
                               BHI.Node (1, 10, [BHI.Node (0, 20, [])])]))
   ```
   matches the pattern `false`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   BHI.isBinomialHeap [BHI.Node (0, 10, []);
                    BHI.Node (2, 5, [BHI.Node (1, 15, []);
                                     BHI.Node (1, 20, [])])]
   ```
   matches the pattern `false`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   BHI.isBinomialTree (BHI.Node (0, 5, [BHI.Node (1, 10, [])]))
   ```
   matches the pattern `false`.

   





## Verify red-black trees
**Note:** These tests may fail if your implementation exhibits reasonably
avoidable issues such as syntax errors, warnings, unbound values, missing
signature constraints, and missing sharing constraints.

For this reason, the point value for these items is low, and most of the points
for this assignment are in the manual assessment. From there you can receive
partial credit for your attempt.

Almost done.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   RBTI.isRedBlackTree
     (RBTI.T (RBTI.B,
              RBTI.T (RBTI.R, RBTI.E, 1, RBTI.E),
              2,
              RBTI.T (RBTI.R,
                      RBTI.T (RBTI.B, RBTI.E, 3, RBTI.E),
                      4,
                      RBTI.T (RBTI.B, RBTI.E, 5, RBTI.E))))
   ```
   matches the pattern `false`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   RBTI.isRedBlackTree
     (RBTI.T (RBTI.B,
              RBTI.T (RBTI.B, RBTI.E, 1, RBTI.E),
              2,
              RBTI.T (RBTI.R,
                      RBTI.T (RBTI.B, RBTI.E, 3, RBTI.E),
                      4,
                      RBTI.T (RBTI.B, RBTI.E, 5, RBTI.E))))
   ```
   matches the pattern `true`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   RBTI.isRedBlackTree
     (RBTI.T (RBTI.B,
              RBTI.T (RBTI.B, RBTI.E, 1, RBTI.E),
              2,
              RBTI.T (RBTI.B,
                      RBTI.T (RBTI.B, RBTI.E, 3, RBTI.E),
                      4,
                      RBTI.T (RBTI.B, RBTI.E, 5, RBTI.E))))
   ```
   matches the pattern `false`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   RBTI.isRedBlackTree
     (RBTI.T (RBTI.B,
              RBTI.T (RBTI.B, RBTI.E, 1, RBTI.E),
              2,
              RBTI.T (RBTI.R,
                      RBTI.T (RBTI.B, RBTI.E, 3, RBTI.E),
                      4,
                      RBTI.T (RBTI.R, RBTI.E, 5, RBTI.E))))
   ```
   matches the pattern `false`.

   




## Extra credit

We will assess this part manually.

#### Total score: _24_ / _25_

