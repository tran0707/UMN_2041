### Assessment for Homework 01

Below are the automated scores for Homework 1.  If you feel that our scripts are incorrectly assessing your work then please email ``csci2041@umn.edu`` and explain the problem.  If your code is right you will get credit for it - just maybe not right away.

Please note that a manual assessment of your code will also be made to look for opportunities for partial credit. Specifically, we will be looking for work that looks close to working but still fails all the tests. There will be some partial credit for that kind of work.

#### Total score: _150_ / _150_

Run on February 14, 22:06:22 PM.

+ Pass: Change into directory "Hwk_01".

### Assessment for Part 1

+ Pass: Check that file "intro.ml" exists.

+  _5_ / _5_ : Pass: Check that an OCaml file "intro.ml" has no syntax or type errors.

    OCaml file "intro.ml" has no syntax or type errors.



+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   even 4
   ```
   matches the pattern `true`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   even 5
   ```
   matches the pattern `false`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   euclid 6 9
   ```
   matches the pattern `3`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   euclid 5 9
   ```
   matches the pattern `1`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   frac_simplify (8,16)
   ```
   matches the pattern `(1,2)`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   frac_simplify (4,9)
   ```
   matches the pattern `(4,9)`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   frac_simplify (3,9)
   ```
   matches the pattern `(1,3)`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   take 3 [1; 2; 3; 4; 5]
   ```
   matches the pattern `[1; 2; 3]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   take 0 ['a'; 'b'; 'c']
   ```
   matches the pattern `[]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   take (-2) ['a'; 'b'; 'c']
   ```
   matches the pattern `[]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   max [1; 2; 5; 3; 2]
   ```
   matches the pattern `5`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   max [-1; -2; -5; -3; -2]
   ```
   matches the pattern `-1`.

   




+  _2_ / _2_ : Pass: Check that the result of evaluating `max []` matches the pattern `Failure`.

   



### Assessment for Part 2

+ Pass: Check that file "higher.ml" exists.

+  _5_ / _5_ : Pass: Check that an OCaml file "higher.ml" has no syntax or type errors.

    OCaml file "higher.ml" has no syntax or type errors.



+ Pass: You are not allowed to use recursion.

   



+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   all_evens [1; 2; 3; 4; 5]
   ```
   matches the pattern `[2; 4]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   all_evens [1; 3; 5]
   ```
   matches the pattern `[]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   increment_all [1; 2; 3; 4; 5]
   ```
   matches the pattern `[2; 3; 4; 5; 6]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   increment_all []
   ```
   matches the pattern `[]`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   max_fold [1; 2; 5; 3; 2]
   ```
   matches the pattern `5`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   max_fold [-1; -2; -5; -3; -2]
   ```
   matches the pattern `-1`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   sum_prod [1; 2; 3]
   ```
   matches the pattern `(6, 6)`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   sum_prod [1; 2; 3; 4]
   ```
   matches the pattern `(10, 24)`.

   




+  _2_ / _2_ : Pass: 
Check that the result of evaluating
   ```
   sum_prod []
   ```
   matches the pattern `(0, 1)`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   split (fun x -> x mod 3 = 0) [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
   ```
   matches the pattern `[[1; 2]; [4; 5]; [7; 8]; [10]]`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   split (fun x -> x > 10) [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
   ```
   matches the pattern `[[1; 2; 3; 4; 5; 6; 7; 8; 9; 10]]`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   split (fun x -> x mod 3 = 0) [1; 2; 3; 3; 4; 5; 6; 6; 6; 7; 8; 9 ;10]
   ```
   matches the pattern `[[1; 2]; []; [4; 5]; []; []; [7; 8]; [10]]`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   split (fun x -> x mod 3 = 0) []
   ```
   matches the pattern `[[]]`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   split (fun x -> String.length x > 4) ["Hello";"how";"are";"you";"?";"this";"looks";"really";"cool"]
   ```
   matches the pattern `[[]; ["how"; "are"; "you"; "?"; "this"]; []; ["cool"]]`.

   




### Assessment for Part 3

+ Pass: Check that file "puzzle.ml" exists.

+  _5_ / _5_ : Pass: Check that an OCaml file "puzzle.ml" has no syntax or type errors.

    OCaml file "puzzle.ml" has no syntax or type errors.



+ Pass: Make sure you are only using recursion in functions read_file, read_chars

   



Note that if your output is in reverse order, it's technically still a correct answer. But to make grading simple, please make sure to match the pattern here.

+  _8_ / _8_ : Pass: 
Check that the result of evaluating
   ```
   answers "../../public-class-repo/Homework/Files/words-small.txt"
   ```
   matches the pattern `["planet"; "smooth"; "change"; "twenty"; "knight"; "street"; "prefix"; "abrade"; "rawest"; "wonton"; "scrawl"; "misled"; "presto"; "upland"; "safari"]`.

   




+  _8_ / _8_ : Pass: 
Check that the result of evaluating
   ```
   answers "../../public-class-repo/Homework/Files/words-google-10000.txt"
   ```
   matches the pattern `["prices"; "sports"; "change"; "course"; "estate"; "street"; "browse"; "casino"; "skills"; "brands"; "places"; "awards"; "closed"; "tables"; "placed"; "covers"; "nearly"; "equity"; "scheme"; "manner"; "proper"; "planet"; "charts"; "cheats"; "tracks"; "cables"; "scores"; "chosen"; "twenty"; "asking"; "blocks"; "banner"; "wheels"; "severe"; "brings"; "thongs"; "shaved"; "chairs"; "spaces"; "aspect"; "priced"; "closer"; "cloudy"; "plates"; "emails"; "stones"; "eminem"; "gloves"; "honest"; "athens"; "tablet"; "tricks"; "lovers"; "ebooks"; "trails"; "sacred"; "oliver"; "scored"; "clouds"; "clocks"; "trains"; "speaks"; "itunes"; "breeds"; "grande"; "freely"; "schema"; "zshops"; "planes"; "charms"; "banned"; "yearly"; "strips"; "blacks"; "closes"; "apollo"; "movers"; "bryant"; "scared"]`.

   




+  _8_ / _8_ : Pass: 
Check that the result of evaluating
   ```
   pretty_answers (answers "../../public-class-repo/Homework/Files/words-small.txt")
   ```
   matches the pattern `[("lane", "planet"); ("moot", "smooth"); ("hang", "change"); ("went", "twenty"); ("nigh", "knight"); ("tree", "street"); ("refi", "prefix"); ("brad", "abrade"); ("awes", "rawest"); ("onto", "wonton"); ("craw", "scrawl"); ("isle", "misled"); ("rest", "presto"); ("plan", "upland"); ("afar", "safari")]`.

   




+  _8_ / _8_ : Pass: 
Check that the result of evaluating
   ```
   pretty_answers (answers "../../public-class-repo/Homework/Files/words-google-10000.txt")
   ```
   matches the pattern `[("rice", "prices"); ("port", "sports"); ("hang", "change"); ("ours", "course"); ("stat", "estate"); ("tree", "street"); ("rows", "browse"); ("asin", "casino"); ("kill", "skills"); ("rand", "brands"); ("lace", "places"); ("ward", "awards"); ("lose", "closed"); ("able", "tables"); ("lace", "placed"); ("over", "covers"); ("earl", "nearly"); ("quit", "equity"); ("chem", "scheme"); ("anne", "manner"); ("rope", "proper"); ("lane", "planet"); ("hart", "charts"); ("heat", "cheats"); ("rack", "tracks"); ("able", "cables"); ("core", "scores"); ("hose", "chosen"); ("went", "twenty"); ("skin", "asking"); ("lock", "blocks"); ("anne", "banner"); ("heel", "wheels"); ("ever", "severe"); ("ring", "brings"); ("hong", "thongs"); ("have", "shaved"); ("hair", "chairs"); ("pace", "spaces"); ("spec", "aspect"); ("rice", "priced"); ("lose", "closer"); ("loud", "cloudy"); ("late", "plates"); ("mail", "emails"); ("tone", "stones"); ("mine", "eminem"); ("love", "gloves"); ("ones", "honest"); ("then", "athens"); ("able", "tablet"); ("rick", "tricks"); ("over", "lovers"); ("book", "ebooks"); ("rail", "trails"); ("acre", "sacred"); ("live", "oliver"); ("core", "scored"); ("loud", "clouds"); ("lock", "clocks"); ("rain", "trains"); ("peak", "speaks"); ("tune", "itunes"); ("reed", "breeds"); ("rand", "grande"); ("reel", "freely"); ("chem", "schema"); ("shop", "zshops"); ("lane", "planes"); ("harm", "charms"); ("anne", "banned"); ("earl", "yearly"); ("trip", "strips"); ("lack", "blacks"); ("lose", "closes"); ("poll", "apollo"); ("over", "movers"); ("ryan", "bryant"); ("care", "scared")]`.

   




### Assessment for Part 4

+ Pass: Check that file "formatter.ml" exists.

+  _5_ / _5_ : Pass: Check that an OCaml file "formatter.ml" has no syntax or type errors.

    OCaml file "formatter.ml" has no syntax or type errors.



+ Pass: Make sure you are only using recursion in functions read_file, read_chars, explode, f

   



Note that you might not see the line breaks in the following patterns, because of the way feedback file is rendered. They are in the actual string.

+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   format p1 12
   ```
   matches the pattern `"Hello world!
How are you
today? I
hope all is
well."`.

   




+  _5_ / _5_ : Pass: 
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

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   format p1 20
   ```
   matches the pattern `"Hello world! How are
you today? I hope
all is well."`.

   




+  _5_ / _5_ : Pass: 
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

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
```
format "Call me Ishmael. Some   years   ago—never  mind 	 how long precisely—having 
little or no money in my purse, and nothing particular to interest me 
on shore, I thought I would sail about a little and see the watery part 
of the world. It is a way I have of driving off the spleen and
regulating the circulation. Whenever I find myself growing grim about
the mouth; whenever it is a damp, drizzly November in my soul; whenever
I find myself involuntarily pausing before coffin warehouses, and 
bringing up the rear of every funeral I meet; and especially whenever
my hypos get such an upper hand of me, that it requires a strong moral 
principle to prevent me from deliberately stepping into the street, and
methodically knocking people’s hats off—then, I account it high time to
get to sea as soon as I can. This is my substitute for pistol and ball.
With a philosophical flourish Cato throws himself upon his sword; I
quietly take to the ship. There is nothing surprising in this. If they
but knew it, almost all men in their degree, some time or other,
cherish very nearly the same feelings towards the ocean with me." 10
```
   matches the pattern 
```
"Call me
Ishmael.
Some years
ago—never
mind how
long
precisely—having
little or
no money
in my
purse, and
nothing
particular
to
interest
me on
shore, I
thought I
would sail
about a
little and
see the
watery
part of
the world.
It is a
way I have
of driving
off the
spleen and
regulating
the
circulation.
Whenever I
find
myself
growing
grim about
the mouth;
whenever
it is a
damp,
drizzly
November
in my
soul;
whenever I
find
myself
involuntarily
pausing
before
coffin
warehouses,
and
bringing
up the
rear of
every
funeral I
meet; and
especially
whenever
my hypos
get such
an upper
hand of
me, that
it
requires a
strong
moral
principle
to prevent
me from
deliberately
stepping
into the
street,
and
methodically
knocking
people’s
hats
off—then,
I account
it high
time to
get to sea
as soon as
I can.
This is my
substitute
for pistol
and ball.
With a
philosophical
flourish
Cato
throws
himself
upon his
sword; I
quietly
take to
the ship.
There is
nothing
surprising
in this.
If they
but knew
it, almost
all men in
their
degree,
some time
or other,
cherish
very
nearly the
same
feelings
towards
the ocean
with me."
```






+  _5_ / _5_ : Pass: 
Check that the result of evaluating
```
format "Call me Ishmael. Some   years   ago—never  mind 	 how long precisely—having 
little or no money in my purse, and nothing particular to interest me 
on shore, I thought I would sail about a little and see the watery part 
of the world. It is a way I have of driving off the spleen and
regulating the circulation. Whenever I find myself growing grim about
the mouth; whenever it is a damp, drizzly November in my soul; whenever
I find myself involuntarily pausing before coffin warehouses, and 
bringing up the rear of every funeral I meet; and especially whenever
my hypos get such an upper hand of me, that it requires a strong moral 
principle to prevent me from deliberately stepping into the street, and
methodically knocking people’s hats off—then, I account it high time to
get to sea as soon as I can. This is my substitute for pistol and ball.
With a philosophical flourish Cato throws himself upon his sword; I
quietly take to the ship. There is nothing surprising in this. If they
but knew it, almost all men in their degree, some time or other,
cherish very nearly the same feelings towards the ocean with me." 50
```
   matches the pattern 
```
"Call me Ishmael. Some years ago—never mind how
long precisely—having little or no money in my
purse, and nothing particular to interest me on
shore, I thought I would sail about a little and
see the watery part of the world. It is a way I
have of driving off the spleen and regulating the
circulation. Whenever I find myself growing grim
about the mouth; whenever it is a damp, drizzly
November in my soul; whenever I find myself
involuntarily pausing before coffin warehouses,
and bringing up the rear of every funeral I meet;
and especially whenever my hypos get such an upper
hand of me, that it requires a strong moral
principle to prevent me from deliberately stepping
into the street, and methodically knocking
people’s hats off—then, I account it high time
to get to sea as soon as I can. This is my
substitute for pistol and ball. With a
philosophical flourish Cato throws himself upon
his sword; I quietly take to the ship. There is
nothing surprising in this. If they but knew it,
almost all men in their degree, some time or
other, cherish very nearly the same feelings
towards the ocean with me."
```






+  _5_ / _5_ : Pass: 
Check that the result of evaluating
```
format "Call me Ishmael. Some   years   ago—never  mind 	 how long precisely—having 
little or no money in my purse, and nothing particular to interest me 
on shore, I thought I would sail about a little and see the watery part 
of the world. It is a way I have of driving off the spleen and
regulating the circulation. Whenever I find myself growing grim about
the mouth; whenever it is a damp, drizzly November in my soul; whenever
I find myself involuntarily pausing before coffin warehouses, and 
bringing up the rear of every funeral I meet; and especially whenever
my hypos get such an upper hand of me, that it requires a strong moral 
principle to prevent me from deliberately stepping into the street, and
methodically knocking people’s hats off—then, I account it high time to
get to sea as soon as I can. This is my substitute for pistol and ball.
With a philosophical flourish Cato throws himself upon his sword; I
quietly take to the ship. There is nothing surprising in this. If they
but knew it, almost all men in their degree, some time or other,
cherish very nearly the same feelings towards the ocean with me." 80
```
   matches the pattern 
```
"Call me Ishmael. Some years ago—never mind how long precisely—having little
or no money in my purse, and nothing particular to interest me on shore, I
thought I would sail about a little and see the watery part of the world. It is
a way I have of driving off the spleen and regulating the circulation. Whenever
I find myself growing grim about the mouth; whenever it is a damp, drizzly
November in my soul; whenever I find myself involuntarily pausing before coffin
warehouses, and bringing up the rear of every funeral I meet; and especially
whenever my hypos get such an upper hand of me, that it requires a strong moral
principle to prevent me from deliberately stepping into the street, and
methodically knocking people’s hats off—then, I account it high time to get
to sea as soon as I can. This is my substitute for pistol and ball. With a
philosophical flourish Cato throws himself upon his sword; I quietly take to the
ship. There is nothing surprising in this. If they but knew it, almost all men
in their degree, some time or other, cherish very nearly the same feelings
towards the ocean with me."
```






#### Total score: _150_ / _150_

