# Hwk_02 Assessment

Run on March 12, 15:05:56 PM.

*Test results are for the code in this repository as it appeared on March 02 at 17:15 CT.*

**Change log:**
+ Friday, March 2: (Sean) Initial version.


## Introduction

### Checking the basics...

+ Pass: Change into directory "Hwk_02".

+ Pass: Check that file "trees.ml" exists.

+  _2_ / _2_ : Pass: Check that an OCaml file "trees.ml" has no syntax or type errors.

    OCaml file "trees.ml" has no syntax or type errors.



+  _3_ / _3_ : Pass: Check that an OCaml file "trees.ml" has warnings.

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

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_size (Leaf 5)
   ```
   matches the pattern `1`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_size (Leaf (- 5))
   ```
   matches the pattern `1`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_size (Fork ("Hello",
       Leaf "World",
       Leaf "!"))
   ```
   matches the pattern `3`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_size (Fork (7,
       Fork (5,
             Leaf 1,
             Leaf 2),
       Fork (6,
             Leaf 3,
             Leaf 4)))
   ```
   matches the pattern `7`.

   




#### 2. ``t_sum``

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_sum (Leaf 5)
   ```
   matches the pattern `5`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_sum (Leaf (- 5))
   ```
   matches the pattern `-5`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_sum (Fork (7,
       Fork (5,
             Leaf 1,
             Leaf 2),
       Fork (6,
             Leaf 3,
             Leaf 4)))
   ```
   matches the pattern `28`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_sum (Fork (0,
       Leaf (- 1),
       Fork(1,
            Leaf 2,
            Leaf (- 2))))
   ```
   matches the pattern `0`.

   




#### 3. ``t_charcount``

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_charcount (Fork ("Hello",
       Leaf "World",
       Leaf "!"))
   ```
   matches the pattern `11`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_charcount (Fork ("a",
       Fork ("b",
             Leaf "c",
             Leaf "d"),
       Leaf "e"))
   ```
   matches the pattern `5`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_charcount (Leaf "a")
   ```
   matches the pattern `1`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_charcount (Fork (":",
       Fork ("-",
             Fork ("D",
                   Leaf "",
                   Leaf ""),
             Leaf ""),
       Leaf ""))
   ```
   matches the pattern `3`.

   




#### 4. ``t_concat``

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_concat (Fork ("Hello",
       Leaf "World",
       Leaf "!"))
   ```
   matches the pattern `"HelloWorld!"`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_concat (Leaf "Hello!")
   ```
   matches the pattern `"Hello!"`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_concat (Leaf "a")
   ```
   matches the pattern `"a"`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_concat (Fork (":",
       Fork ("-",
             Fork ("D",
                   Leaf "",
                   Leaf ""),
             Leaf ""),
       Leaf ""))
   ```
   matches the pattern `":-D"`.

   




## Part B

Just like in Gradius, you'll find options quite useful.

#### 1. ``t_opt_size``

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_opt_size (Fork (Some "Hello",
       Leaf (Some "World"),
       Leaf None))
   ```
   matches the pattern `2`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_opt_size (Fork (Some 1, Leaf (Some 2), Fork (Some 3, Leaf None, Leaf None)))
   ```
   matches the pattern `3`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_opt_size (Fork (Some "a", Leaf (Some "b"), Fork (Some "c", Leaf None, Leaf (Some "d"))))
   ```
   matches the pattern `4`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_opt_size (Leaf None)
   ```
   matches the pattern `0`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_opt_size (Fork (None, (Leaf None), (Leaf None)))
   ```
   matches the pattern `0`.

   




#### 2. ``t_opt_sum``

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_opt_sum (Leaf None)
   ```
   matches the pattern `0`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_opt_sum (Fork (Some 1, Leaf (Some 2), Fork (Some 3, Leaf None, Leaf None)))
   ```
   matches the pattern `6`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_opt_sum (Fork (None, (Leaf None), (Leaf None)))
   ```
   matches the pattern `0`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_opt_sum (Leaf None)
   ```
   matches the pattern `0`.

   




#### 3. ``t_opt_charcount``

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_opt_charcount (Fork (Some "Hello",
       Leaf (Some "World"),
       Leaf None))
   ```
   matches the pattern `10`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_opt_charcount (Leaf None)
   ```
   matches the pattern `0`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_opt_charcount (Leaf (Some "abcd"))
   ```
   matches the pattern `4`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_opt_charcount (Fork (Some "a", Leaf (Some "b"), Fork (Some "c", Leaf None, Leaf (Some "d"))))
   ```
   matches the pattern `4`.

   




#### 4. ``t_opt_concat``

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_opt_concat (Fork (Some "Hello",
       Leaf (Some "World"),
       Leaf None))
   ```
   matches the pattern `"HelloWorld"`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_opt_concat (Leaf None)
   ```
   matches the pattern `""`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_opt_concat (Leaf (Some "abcd"))
   ```
   matches the pattern `"abcd"`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   t_opt_concat (Fork (Some "a", Leaf (Some "b"), Fork (Some "c", Leaf None, Leaf (Some "d"))))
   ```
   matches the pattern `"abcd"`.

   




## Part C

### All these functions must be non-recursive.

#### 1. ``tf_size``

+ Pass: Check if `tf_size` is non-recursive.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_size (Fork ("Hello",
       Leaf "World",
       Leaf "!"))
   ```
   matches the pattern `3`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_size (Fork ("Hello", Leaf "World", Leaf "!"))
   ```
   matches the pattern `3`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_size (Fork (7, Fork (5, Leaf 1, Leaf 2), Fork (6, Leaf 3, Leaf 4)))
   ```
   matches the pattern `7`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_size (Leaf 5)
   ```
   matches the pattern `1`.

   




#### 2. ``tf_sum``

+ Pass: Check if `tf_sum` is non-recursive.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_sum (Leaf 5)
   ```
   matches the pattern `5`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_sum (Leaf (- 5))
   ```
   matches the pattern `-5`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_sum (Fork (0, Leaf (- 1), Fork(1, Leaf 2, (Leaf (- 2)))))
   ```
   matches the pattern `0`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_sum (Fork (7, Fork (5, Leaf 1, Leaf 2), Fork (6, Leaf 3, Leaf 4)))
   ```
   matches the pattern `28`.

   




#### 3. ``tf_charcount``

+ Pass: Check if `tf_charcount` is non-recursive.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_charcount (Fork ("Hello",
       Leaf "World",
       Leaf "!"))
   ```
   matches the pattern `11`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_charcount (Fork (":",
       Fork ("-",
             Fork ("D",
                   Leaf "",
                   Leaf ""),
             Leaf ""),
       Leaf ""))
   ```
   matches the pattern `3`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_charcount (Fork ("a", Fork ("b", Leaf "c", Leaf "d"), Leaf "e"))
   ```
   matches the pattern `5`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_charcount (Leaf "a")
   ```
   matches the pattern `1`.

   




#### 4. ``tf_concat``

+ Pass: Check if `tf_concat` is non-recursive.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_concat (Fork ("Hello",
       Leaf "World",
       Leaf "!"))
   ```
   matches the pattern `"HelloWorld!"`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_concat (Leaf "Hello!")
   ```
   matches the pattern `"Hello!"`.

   




#### 5. ``tf_opt_size``

+ Pass: Check if `tf_opt_size` is non-recursive.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_opt_size (Fork (Some "Hello",
       Leaf (Some "World"),
       Leaf None))
   ```
   matches the pattern `2`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_opt_size (Fork (Some 1, Leaf (Some 2), Fork (Some 3, Leaf None, Leaf None)))
   ```
   matches the pattern `3`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_opt_size (Fork (Some "a", Leaf (Some "b"), Fork (Some "c", Leaf None, Leaf (Some "d"))))
   ```
   matches the pattern `4`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_opt_size (Leaf None)
   ```
   matches the pattern `0`.

   




+  _0_ / _1_ : Fail: 
Check that the result of evaluating
   ```
   tf_opt_size (Fork (None, (Leaf None), (Leaf None)))
   ```
   matches the pattern `0`.

   


   Your solution evaluated incorrectly and produced some part of the following:

    ```
    - : int = 1
    ```


#### 6. ``tf_opt_sum``

+ Pass: Check if `tf_opt_sum` is non-recursive.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_opt_sum (Leaf None)
   ```
   matches the pattern `0`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_opt_sum (Fork (Some 1, Leaf (Some 2), Fork (Some 3, Leaf None, Leaf None)))
   ```
   matches the pattern `6`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_opt_sum (Fork (None, (Leaf None), (Leaf None)))
   ```
   matches the pattern `0`.

   




#### 7. ``tf_opt_charcount``

+ Pass: Check if `tf_opt_charcount` is non-recursive.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_opt_charcount (Fork (Some "Hello",
       Leaf (Some "World"),
       Leaf None))
   ```
   matches the pattern `10`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_opt_charcount (Leaf None)
   ```
   matches the pattern `0`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_opt_charcount (Leaf (Some "abcd"))
   ```
   matches the pattern `4`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_opt_charcount (Fork (Some "a", Leaf (Some "b"), Fork (Some "c", Leaf None, Leaf (Some "d"))))
   ```
   matches the pattern `4`.

   




#### 8. ``tf_opt_concat``

+ Pass: Check if `tf_opt_concat` is non-recursive.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_opt_concat (Fork (Some "Hello",
       Leaf (Some "World"),
       Leaf None))
   ```
   matches the pattern `"HelloWorld"`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_opt_concat (Leaf None)
   ```
   matches the pattern `""`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_opt_concat (Leaf (Some "abcd"))
   ```
   matches the pattern `"abcd"`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   tf_opt_concat (Fork (Some "a", Leaf (Some "b"), Fork (Some "c", Leaf None, Leaf (Some "d"))))
   ```
   matches the pattern `"abcd"`.

   




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

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   bt_insert_by Pervasives.compare 3 Empty
   ```
   matches the pattern `Node (Empty, 3, Empty)`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   bt_insert_by Pervasives.compare 2 (bt_insert_by Pervasives.compare 4 (bt_insert_by Pervasives.compare 3 Empty))
   ```
   matches the pattern `Node (Node (Empty, 2, Empty), 3, Node (Empty, 4, Empty))`.

   




+  _0_ / _1_ : Fail: 
Check that the result of evaluating
   ```
   bt_insert_by Pervasives.compare 3 (Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Empty)))
   ```
   matches the pattern `Node (Node (Node (Empty, 3, Empty), 3, Empty), 4, Node (Empty, 5, Empty))`.

   


   Your solution evaluated incorrectly and produced some part of the following:

    ```
    - : int btree = Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Empty))
    ```


+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   bt_insert_by Pervasives.compare 6 (Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Empty)))
   ```
   matches the pattern `Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Node (Empty, 6, Empty)))`.

   




#### 2. ``bt_elem_by``

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   bt_elem_by (=) 5 Empty
   ```
   matches the pattern `false`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   bt_elem_by (=) 6 (Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Node (Empty, 6, Empty))));
   ```
   matches the pattern `true`.

   




+  _0_ / _1_ : Fail: 
Check that the result of evaluating
   ```
   bt_elem_by (<) 6 (Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Node (Empty, 6, Empty))))
   ```
   matches the pattern `true`.

   


   Your solution evaluated incorrectly and produced some part of the following:

    ```
    - : bool = false
    ```


+  _0_ / _1_ : Fail: 
Check that the result of evaluating
   ```
   bt_elem_by (>) 6 (Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Node (Empty, 6, Empty))))
   ```
   matches the pattern `false`.

   


   Your solution evaluated incorrectly and produced some part of the following:

    ```
    - : bool = true
    ```


#### 3. ``bt_to_list``

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   bt_to_list (Empty)
   ```
   matches the pattern `[]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   bt_to_list (Node (Empty, 3, Empty))
   ```
   matches the pattern `[3]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   bt_to_list (Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Node (Empty, 6, Empty))))
   ```
   matches the pattern `[3; 4; 5; 6]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   bt_to_list (Node (Node (Empty, "a",  Empty), "b", Node (Empty, "c",  Node (Empty, "d", Empty))))
   ```
   matches the pattern `["a"; "b"; "c"; "d"]`.

   




#### 4. ``btfold``

No assessment for this specifically; See the tests below.

### Non-recursive `btf` functions:

#### 5. ``btf_to_list``

+ Pass: Check if `btf_to_list` is non-recursive.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   btf_to_list (Empty)
   ```
   matches the pattern `[]`.

   




+  _1_ / _1_ : Pass: 
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

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   btf_to_list (Node (Empty, 3, Empty))
   ```
   matches the pattern `[3]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   btf_to_list (Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Node (Empty, 6, Empty))))
   ```
   matches the pattern `[3; 4; 5; 6]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   btf_to_list (Node (Node (Empty, "a",  Empty), "b", Node (Empty, "c",  Node (Empty, "d", Empty))))
   ```
   matches the pattern `["a"; "b"; "c"; "d"]`.

   




#### 6. ``btf_elem_by``

+ Pass: Check if `btf_elem_by` is non-recursive.

+  _1_ / _1_ : Pass: 
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

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   btf_elem_by (=) 5 Empty
   ```
   matches the pattern `false`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
    btf_elem_by (=) 6 (Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Node (Empty, 6, Empty))));
   ```
   matches the pattern `true`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   btf_elem_by (<) 6 (Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Node (Empty, 6, Empty))))
   ```
   matches the pattern `true`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   btf_elem_by (>) 6 (Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Node (Empty, 6, Empty))))
   ```
   matches the pattern `false`.

   




#### 7. ``btf_insert_by``

+  _10_ / _10_ : Pass: Check for comment on why `btf_insert_by` might be difficult.

    

## End 🤖📄💁

#### Total score: _96_ / _100_

