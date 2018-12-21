### Assessment for Lab 05

#### Total score: _48_ / _48_

Run on March 08, 00:14:48 AM.

+ Pass: Change into directory "Lab_05".

+ Pass: Check that file "higher.ml" exists.

+ Pass: Check that an OCaml file "higher.ml" has no syntax or type errors.

    OCaml file "higher.ml" has no syntax or type errors.



+  _1_ / _1_ : Pass: Check that file higher.ml does not contain lines of code over 100 characters wide.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   split (fun x -> x mod 3 = 0) [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
   ```
   matches the pattern `[[1; 2]; [4; 5]; [7; 8]; [10]]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   split (fun x -> x > 10) [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
   ```
   matches the pattern `[[1; 2; 3; 4; 5; 6; 7; 8; 9; 10]]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   split (fun x -> x mod 3 = 0) [1; 2; 3; 3; 4; 5; 6; 6; 6; 7; 8; 9 ;10]
   ```
   matches the pattern `[[1; 2]; []; [4; 5]; []; []; [7; 8]; [10]]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   split (fun x -> String.length x > 4) ["Hello";"how";"are";"you";"?";"this";"looks";"really";"cool"]
   ```
   matches the pattern `[[]; ["how"; "are"; "you"; "?"; "this"]; []; ["cool"]]`.

   




+ Pass: Check that file "puzzle.ml" exists.

+ Pass: Check that an OCaml file "puzzle.ml" has no syntax or type errors.

    OCaml file "puzzle.ml" has no syntax or type errors.



+ Pass: Make sure you are only using recursion in functions read_file, read_chars

   



+  _1_ / _1_ : Pass: Check that file puzzle.ml does not contain lines of code over 100 characters wide.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   answers "/home/telan024/csci2041/spring_18/public-class-repo/Homework/Files/words-small.txt"
   ```
   matches the pattern `["planet"; "smooth"; "change"; "twenty"; "knight"; "street"; "prefix"; "abrade"; "rawest"; "wonton"; "scrawl"; "misled"; "presto"; "upland"; "safari"]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   answers "/home/telan024/csci2041/spring_18/public-class-repo/Homework/Files/words-google-10000.txt"
   ```
   matches the pattern `["prices"; "sports"; "change"; "course"; "estate"; "street"; "browse"; "casino"; "skills"; "brands"; "places"; "awards"; "closed"; "tables"; "placed"; "covers"; "nearly"; "equity"; "scheme"; "manner"; "proper"; "planet"; "charts"; "cheats"; "tracks"; "cables"; "scores"; "chosen"; "twenty"; "asking"; "blocks"; "banner"; "wheels"; "severe"; "brings"; "thongs"; "shaved"; "chairs"; "spaces"; "aspect"; "priced"; "closer"; "cloudy"; "plates"; "emails"; "stones"; "eminem"; "gloves"; "honest"; "athens"; "tablet"; "tricks"; "lovers"; "ebooks"; "trails"; "sacred"; "oliver"; "scored"; "clouds"; "clocks"; "trains"; "speaks"; "itunes"; "breeds"; "grande"; "freely"; "schema"; "zshops"; "planes"; "charms"; "banned"; "yearly"; "strips"; "blacks"; "closes"; "apollo"; "movers"; "bryant"; "scared"]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   pretty_answers (answers "/home/telan024/csci2041/spring_18/public-class-repo/Homework/Files/words-small.txt")
   ```
   matches the pattern `[("lane", "planet"); ("moot", "smooth"); ("hang", "change"); ("went", "twenty"); ("nigh", "knight"); ("tree", "street"); ("refi", "prefix"); ("brad", "abrade"); ("awes", "rawest"); ("onto", "wonton"); ("craw", "scrawl"); ("isle", "misled"); ("rest", "presto"); ("plan", "upland"); ("afar", "safari")]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   pretty_answers (answers "/home/telan024/csci2041/spring_18/public-class-repo/Homework/Files/words-google-10000.txt")
   ```
   matches the pattern `[("rice", "prices"); ("port", "sports"); ("hang", "change"); ("ours", "course"); ("stat", "estate"); ("tree", "street"); ("rows", "browse"); ("asin", "casino"); ("kill", "skills"); ("rand", "brands"); ("lace", "places"); ("ward", "awards"); ("lose", "closed"); ("able", "tables"); ("lace", "placed"); ("over", "covers"); ("earl", "nearly"); ("quit", "equity"); ("chem", "scheme"); ("anne", "manner"); ("rope", "proper"); ("lane", "planet"); ("hart", "charts"); ("heat", "cheats"); ("rack", "tracks"); ("able", "cables"); ("core", "scores"); ("hose", "chosen"); ("went", "twenty"); ("skin", "asking"); ("lock", "blocks"); ("anne", "banner"); ("heel", "wheels"); ("ever", "severe"); ("ring", "brings"); ("hong", "thongs"); ("have", "shaved"); ("hair", "chairs"); ("pace", "spaces"); ("spec", "aspect"); ("rice", "priced"); ("lose", "closer"); ("loud", "cloudy"); ("late", "plates"); ("mail", "emails"); ("tone", "stones"); ("mine", "eminem"); ("love", "gloves"); ("ones", "honest"); ("then", "athens"); ("able", "tablet"); ("rick", "tricks"); ("over", "lovers"); ("book", "ebooks"); ("rail", "trails"); ("acre", "sacred"); ("live", "oliver"); ("core", "scored"); ("loud", "clouds"); ("lock", "clocks"); ("rain", "trains"); ("peak", "speaks"); ("tune", "itunes"); ("reed", "breeds"); ("rand", "grande"); ("reel", "freely"); ("chem", "schema"); ("shop", "zshops"); ("lane", "planes"); ("harm", "charms"); ("anne", "banned"); ("earl", "yearly"); ("trip", "strips"); ("lack", "blacks"); ("lose", "closes"); ("poll", "apollo"); ("over", "movers"); ("ryan", "bryant"); ("care", "scared")]`.

   




+ Pass: Check that file "formatter.ml" exists.

+ Pass: Check that an OCaml file "formatter.ml" has no syntax or type errors.

    OCaml file "formatter.ml" has no syntax or type errors.



+ Pass: Make sure you are only using recursion in functions read_file, read_chars, explode, f

   



+  _1_ / _1_ : Pass: Check that file formatter.ml does not contain lines of code over 100 characters wide.

+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   format p1 12
   ```
   matches the pattern `"Hello world!
How are you
today? I
hope all is
well."`.

   




+  _1_ / _1_ : Pass: 
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

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   format p1 20
   ```
   matches the pattern `"Hello world! How are
you today? I hope
all is well."`.

   




+  _1_ / _1_ : Pass: 
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

   




+  _1_ / _1_ : Pass: 
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
   matches the pattern `"Call me
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
with me."`.

   




+  _1_ / _1_ : Pass: 
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
   matches the pattern `"Call me Ishmael. Some years ago—never mind how
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
towards the ocean with me."`.

   




+  _1_ / _1_ : Pass: 
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
   matches the pattern `"Call me Ishmael. Some years ago—never mind how long precisely—having little
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
towards the ocean with me."`.

   




+  _3_ / _3_ : Pass: Check if prologue comment has group member names

    

+  _9_ / _9_ : Pass: Check if at least two of the three functions have type annotation

    Type annotation should be a part of function, not in comment

+  _9_ / _9_ : Pass: Check if code has explanatory comments.

    

+  _9_ / _9_ : Pass: Check if code is readable and has consistent style.

    

#### Total score: _48_ / _48_

