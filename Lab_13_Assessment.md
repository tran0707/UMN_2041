### Assessment for Lab 13

#### Total score: _40_ / _40_

Run on April 28, 14:54:42 PM.

+ Pass: Change into directory "Lab_13".

+ Pass: Check that file "binomialheap.ml" exists.

+ Pass: Check that an OCaml file "binomialheap.ml" has no syntax or type errors.

    OCaml file "binomialheap.ml" has no syntax or type errors.



+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   m1
   ```
   matches the pattern `10`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   m2
   ```
   matches the pattern `20`.

   




+  _10_ / _10_ : Pass: 
Check that the result of evaluating
   ```
   BHI.findMin (BHI.insert 4 (BHI.insert 8 (BHI.insert 6 (BHI.insert 2 BHI.empty))))
   ```
   matches the pattern `2`.

   




+  _10_ / _10_ : Pass: 
Check that the result of evaluating
   ```
   BHI.merge (BHI.insert 5 (BHI.insert 15 (BHI.insert (-2) BHI.empty))) (BHI.insert 6 (BHI.insert (-11) (BHI.insert (-2) (BHI.insert (-7) (BHI.insert (-15) (BHI.insert 6 BHI.empty))))))
   ```
   matches the pattern `[BHI.Node (0, 5, []); BHI.Node (3, (-15), [BHI.Node (2, (-11), [BHI.Node (1, (-2), [BHI.Node (0, 15, [])]); BHI.Node (0,6,[])]); BHI.Node (1, (-7), [BHI.Node (0, (-2), [])]); BHI.Node (0, 6, [])])]`.

   




+  _10_ / _10_ : Pass: 
Check that the result of evaluating
   ```
   BHI.deleteMin [BHI.Node (0, 5, []); BHI.Node (3, (-15), [BHI.Node (2, (-11), [BHI.Node (1, (-2), [BHI.Node (0, 15, [])]); BHI.Node (0,6,[])]); BHI.Node (1, (-7), [BHI.Node (0, (-2), [])]); BHI.Node (0, 6, [])])]
   ```
   matches the pattern `[BHI.Node (3, -11, [BHI.Node (2, -7, [BHI.Node (1, 5, [BHI.Node (0, 6, [])]); BHI.Node (0, -2, [])]); BHI.Node (1, -2, [BHI.Node (0, 15, [])]); BHI.Node (0, 6, [])])] `.

   




