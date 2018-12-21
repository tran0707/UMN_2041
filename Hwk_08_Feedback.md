## Feedback for Homework 08

Run on May 01, 23:59:40 PM.

## Feedback for Homework 8

+ Pass: Check that directory "Hwk_08" exists.

+ Pass: Change into directory "Hwk_08".

### Test for binomial heaps

+ Pass: Check that file "ordered.ml" exists.

+ Pass: Check that the command ``ocamlbuild ordered.byte`` executes with return code of 0.

    The command succeeded.

+ Pass: Check that file "binomialheap.ml" exists.

+ Pass: Check that the command ``ocamlbuild binomialheap.byte`` executes with return code of 0.

    The command succeeded.

### Tests for Lab 14

+ Fail: 
Check that the result of evaluating
   ```
   BHI.findMin (List.fold_left (fun h e -> BHI.insert e h) BHI.empty [4; 8; 6; 2; 10; 5])
   ```
   matches the pattern `2`.

   


   Test failed. The following errors were reported:
   ```
    !!Error in evaluating expressio
    ```

+ Pass: 
Check that the result of evaluating
   ```
   BHI.findMinDirect (List.fold_left (fun h e -> BHI.insert e h) BHI.empty [4; 8; 6; 2; 10; 5])
   ```
   matches the pattern `2`.

   




### Tests for Hwk 08

+ Pass: 
Check that the result of evaluating
   ```
   BHI.isBinomialTree (BHI.Node (0, 10, []))
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   BHI.isBinomialTree (BHI.Node (1, 5, [BHI.Node (0, 10, [])]))
   ```
   matches the pattern `true`.

   




+ Fail: 
Check that the result of evaluating
   ```
   BHI.isBinomialTree (BHI.Node (2, 5, [BHI.Node (0, 10, [])]))
   ```
   matches the pattern `false`.

   


   Test failed. The following errors were reported:
   ```
    !!Error in evaluating expressio
    ```

+ Pass: 
Check that the result of evaluating
   ```
   BHI.isBinomialTree (BHI.Node (2, 5, [BHI.Node (0, 1, [])]))
   ```
   matches the pattern `false`.

   




+ Pass: 
Check that the result of evaluating
   ```
   BHI.isBinomialHeap [ BHI.Node (0, 10, []); BHI.Node (1, 5, [BHI.Node (0, 15, [])]) ]
   ```
   matches the pattern `true`.

   




### Test for red black trees

+ Pass: Check that file "redblacktree.ml" exists.

+ Pass: Check that the command ``ocamlbuild redblacktree.byte`` executes with return code of 0.

    The command succeeded.

+ Fail: 
Check that the result of evaluating
   ```
   RBTI.member 3 (List.fold_left (fun t e -> RBTI.insert e t) RBTI.E [1;2;3;4;5])
   ```
   matches the pattern `true`.

   


   Test failed. The following errors were reported:
   ```
    !!Error in evaluating expressio
    ```

+ Fail: 
Check that the result of evaluating
   ```
   RBTI.member 8 (List.fold_left (fun t e -> RBTI.insert e t) RBTI.E [1;2;3;4;5])
   ```
   matches the pattern `false`.

   


   Test failed. The following errors were reported:
   ```
    !!Error in evaluating expressio
    ```

+ Fail: 
Check that the result of evaluating
   ```
   RBTI.isRedBlackTree (RBTI.T (RBTI.B, RBTI.T (RBTI.B, RBTI.E, 1, RBTI.E), 2, RBTI.T (RBTI.R, RBTI.T (RBTI.B, RBTI.E, 3, RBTI.E), 4, RBTI.T (RBTI.B, RBTI.E, 5, RBTI.E))))
   ```
   matches the pattern `true`.

   


   Test failed. The following errors were reported:
   ```
    !!Error in evaluating expressio
    ```

+ Fail: 
Check that the result of evaluating
   ```
   RBTI.isRedBlackTree (RBTI.T (RBTI.B, RBTI.T (RBTI.B, RBTI.E, 1, RBTI.E), 2, RBTI.T (RBTI.R, RBTI.T (RBTI.B, RBTI.E, 3, RBTI.E), 4, RBTI.T (RBTI.R, RBTI.E, 5, RBTI.E))))
   ```
   matches the pattern `false`.

   


   Test failed. The following errors were reported:
   ```
    !!Error in evaluating expressio
    ```

