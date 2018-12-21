### Assessment for Lab 14

#### Total score: _20_ / _20_

Run on April 28, 09:06:49 AM.

## Assessment for Lab 14

+ Pass: Check that directory "Hwk_08" exists.

+ Pass: Change into directory "Hwk_08".

+ Pass: Check that file "ordered.ml" exists.

+ Pass: Check that file "binomialheap.ml" exists.

+  _10_ / _10_ : Pass: 
Check that the result of evaluating
   ```
   true
   ```
   matches the pattern `true`.

   Note: This test just checks that binomialheap.ml can be loaded without errors.




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   BHI.findMinDirect (List.fold_left (fun h e -> BHI.insert e h) BHI.empty [4; 8; 6; 2; 10; 5])
   ```
   matches the pattern `2`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   BHI.findMinDirect (BHI.insert 2 BHI.empty)
   ```
   matches the pattern `2`.

   




#### Total score: _20_ / _20_

