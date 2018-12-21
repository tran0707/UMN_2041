# Hwk_02 Feedback

Run on February 27, 13:26:39 PM.

**Changelog:**
+ Friday, February 23: (Sean) Initial version.


## Introduction

### Checking the basics...

+ Pass: Change into directory "Hwk_02".

+ Pass: Check that file "trees.ml" exists.

+ Pass: Check that an OCaml file "trees.ml" has no syntax or type errors.

    OCaml file "trees.ml" has no syntax or type errors.



+ Pass: Check that an OCaml file "trees.ml" has warnings.

    OCaml file "trees.ml" has no warnings.



## Part A

### Sanity check: Can we make a tree?

+ Pass: 
Check that the result of evaluating
   ```
   (Leaf 5)
   ```
   matches the pattern `(Leaf 5)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   (Fork ("Hello",
       Leaf "World",
       Leaf "!"))
   ```
   matches the pattern `(Fork ("Hello",
       Leaf "World",
       Leaf "!"))`.

   




_Other sensible sanity checks: A tree which contains two forks, a string tree, ... maybe a tree of trees?_

### Let's try out your functions:

#### 1. ``t_size``

+ Pass: 
Check that the result of evaluating
   ```
   t_size (Fork ("Hello",
       Leaf "World",
       Leaf "!"))
   ```
   matches the pattern `3`.

   




#### 2. ``t_sum``

+ Pass: 
Check that the result of evaluating
   ```
   t_sum (Leaf 5)
   ```
   matches the pattern `5`.

   




+ Pass: 
Check that the result of evaluating
   ```
   t_sum (Leaf (- 5))
   ```
   matches the pattern `-5`.

   




#### 3. ``t_charcount``

+ Pass: 
Check that the result of evaluating
   ```
   t_charcount (Fork ("Hello",
       Leaf "World",
       Leaf "!"))
   ```
   matches the pattern `11`.

   




#### 4. ``t_concat``

+ Pass: 
Check that the result of evaluating
   ```
   t_concat (Fork ("Hello",
       Leaf "World",
       Leaf "!"))
   ```
   matches the pattern `"HelloWorld!"`.

   




## Part B

Just like in Gradius, you'll find options quite useful.

#### 1. ``t_opt_size``

+ Pass: 
Check that the result of evaluating
   ```
   t_opt_size (Fork (Some "Hello",
       Leaf (Some "World"),
       Leaf None))
   ```
   matches the pattern `2`.

   




#### 2. ``t_opt_sum``

+ Pass: 
Check that the result of evaluating
   ```
   t_opt_sum (Leaf None)
   ```
   matches the pattern `0`.

   




#### 3. ``t_opt_charcount``

+ Pass: 
Check that the result of evaluating
   ```
   t_opt_charcount (Fork (Some "Hello",
       Leaf (Some "World"),
       Leaf None))
   ```
   matches the pattern `10`.

   




#### 4. ``t_opt_concat``

+ Pass: 
Check that the result of evaluating
   ```
   t_opt_concat (Fork (Some "Hello",
       Leaf (Some "World"),
       Leaf None))
   ```
   matches the pattern `"HelloWorld"`.

   




## Part C

### Check it before you rec it!

+ Pass: Check if tf_opt_concat (for example) is non-recursive.

#### 1. ``tf_size``

+ Pass: 
Check that the result of evaluating
   ```
   tf_size (Fork ("Hello",
       Leaf "World",
       Leaf "!"))
   ```
   matches the pattern `3`.

   




#### 2. ``tf_sum``

+ Pass: 
Check that the result of evaluating
   ```
   tf_sum (Leaf 5)
   ```
   matches the pattern `5`.

   




+ Pass: 
Check that the result of evaluating
   ```
   tf_sum (Leaf (- 5))
   ```
   matches the pattern `-5`.

   




#### 3. ``tf_charcount``

+ Pass: 
Check that the result of evaluating
   ```
   tf_charcount (Fork ("Hello",
       Leaf "World",
       Leaf "!"))
   ```
   matches the pattern `11`.

   




#### 4. ``tf_concat``

+ Pass: 
Check that the result of evaluating
   ```
   tf_concat (Fork ("Hello",
       Leaf "World",
       Leaf "!"))
   ```
   matches the pattern `"HelloWorld!"`.

   




#### 5. ``tf_opt_size``

+ Pass: 
Check that the result of evaluating
   ```
   tf_opt_size (Fork (Some "Hello",
       Leaf (Some "World"),
       Leaf None))
   ```
   matches the pattern `2`.

   




#### 6. ``tf_opt_sum``

+ Pass: 
Check that the result of evaluating
   ```
   tf_opt_sum (Leaf None)
   ```
   matches the pattern `0`.

   




#### 7. ``tf_opt_charcount``

+ Pass: 
Check that the result of evaluating
   ```
   tf_opt_charcount (Fork (Some "Hello",
       Leaf (Some "World"),
       Leaf None))
   ```
   matches the pattern `10`.

   




#### 8. ``tf_opt_concat``

+ Pass: 
Check that the result of evaluating
   ```
   tf_opt_concat (Fork (Some "Hello",
       Leaf (Some "World"),
       Leaf None))
   ```
   matches the pattern `"HelloWorld"`.

   




## Part D

### Sanity check: Can we make a sorted tree?

+ Pass: 
Check that the result of evaluating
   ```
   (Empty)
   ```
   matches the pattern `(Empty)`.

   




+ Pass: 
Check that the result of evaluating
   ```
   (Node (Node (Empty,
             "Hello",
             Empty),
       "World",
       Node (Empty,
             "!",
             Empty)))
   ```
   matches the pattern `(Node (Node (Empty,
             "Hello",
             Empty),
       "World",
       Node (Empty,
             "!",
             Empty)))`.

   




### Basic `bt` functions:

#### 1. ``bt_insert_by``

+ Pass: 
Check that the result of evaluating
   ```
   bt_insert_by Pervasives.compare 3 Empty
   ```
   matches the pattern `Node (Empty, 3, Empty)`.

   




#### 2. ``bt_elem_by``

+ Pass: 
Check that the result of evaluating
   ```
   bt_elem_by (=) 5 Empty
   ```
   matches the pattern `false`.

   




#### 3. ``bt_to_list``

+ Pass: 
Check that the result of evaluating
   ```
   bt_to_list (Empty)
   ```
   matches the pattern `[]`.

   




### Non-recursive `btf` functions:

Quick check:

+ Pass: Check if btf_elem_by (for example) is non-recursive.

#### 4. ``btfold``

No feedback for this specifically; See the tests below.

#### 5. ``btf_to_list``

+ Pass: 
Check that the result of evaluating
   ```
   btf_to_list (Empty)
   ```
   matches the pattern `[]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   btf_to_list (Node (Node (Empty,
             "Hello",
             Empty),
       "World",
       Node (Empty,
             "!",
             Empty)))
   ```
   matches the pattern `["Hello"; "World"; "!"]`.

   




#### 6. ``btf_elem_by``

+ Pass: 
Check that the result of evaluating
   ```
   btf_elem_by (=) "!" (Node (Node (Empty,
             "Hello",
             Empty),
       "World",
       Node (Empty,
             "!",
             Empty)))
   ```
   matches the pattern `true`.

   




## End 🤖

