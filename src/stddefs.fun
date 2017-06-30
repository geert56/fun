/* This file contains the standard definitions for the FUN program.
   Names defined below may be redefined by the user; this will cause
   the program to issue a warning message, unless the name is first
   `undef'-ed. Names that are redefined will not affect any other
   definitions that use them.
   The definitions are documented so that they may serve as examples
   of the use of the FUN language for the beginning programmer.
   On purpose, different styles of using the language are exhibited.
   Of several functions we present alternative ways of defining them;
   this to stress the richness and possibilities of the language and
   thereby also informally explaining the various syntactic contructs.
   For more examples check the "ex" directory.

   Have FUN! (TM) Copyright (c) 1995-1996, G. Janssen.

   TM "Have FUN!" is claimed by me as a trademark.
*/

/* Definitions of various combinators:
   These come in handy when a user wants to experiment with combinators
   directly. In general, any lambda expression input to the FUN program
   will be compiled into these combinators and then lazily reduced.
*/
let I  = L x       . x;
let K  = L x y     . x;
let S  = L x y z   . x z (y z);
let B  = L x y z   . x (y z);
let C  = L x y z   . x z y;
let S' = L x y z u . x (y u) (z u);
let B* = L x y z u . x (y (z u));
let C' = L x y z u . x (y u) z;
/* The famous Y-combinator which finds a fixed-point of a function, and
   thus is used to define functions recursively. A letrec is compiled as
   follows: letrec f = E ==> Y L f . E
*/
let Y  = L f       . let D = L x . f (x x) in D D;

/* One could also define the lambda-calculus based on a single combinator:
   X = L v . v K S K, or
   X = L v . v (L x y . x) (L x y z . x z (y z)) (L x y . x)
   then X X X = K and X (X X) = S.
*/

/* Miranda (TM) names for combinators:
   (TM Miranda is a trademark of Research Software Limited.)
*/
let       id = I;
let    const = K;
let  compose = B;
let converse = C;

/* GENERAL PURPOSE: */

/* The built-in `shows' appends its two arguments after first fully evaluating
   its first argument and representing it as a string (list of chars).
   Only boolean truth-values, chars, numbers, and strings have valid string
   representations. Our `show' defined here, therefore, can be used to turn
   a boolean, char, numbers, or string into a string.
   E.g. show (&& False True) ==> "False"
        show (chr 10)         ==> "'\\n'" (note the explicit presence of \)
        show (+ 1 2)          ==> "3"
        show "hello"          ==> "\"hello\""
        show Nil = show [] = show "" ==> []
*/
let show = L atom . shows atom "";

/* BOOLEANs */

/* Aliases: */
let     not = !;
/* Have better uses for these, see below.
let     and = &&;
let      or = ||;
*/
let implies = ->;
let     xor = !=;
let   equiv = ==;

/* Useful as syntactic sugar in if-expressions:
   (They can then be written for example as
      (if (null s) []
      (otherwise   (do-something s)))
   )
*/
let otherwise = I;

/* TUPLEs */
/* Tuples are simply implemented as Nil terminated lists. */
let fst = car;
let snd = car . cdr;

/* LISTs */

/* Lists need not have their elements of the same type, unlike in Miranda.
   FUN lists are more like Lisp lists. You can even have a so-called dotted
   list, i.e., the cdr of the last Cons cell need not necessarily be Nil.

   Note: the reserved word `Nil' and the empty list [] may be used
         interchangeably. Moreover, as we shall see shortly, the empty
	 string "" is represented by an empty list.
	 Also, the following are equivalent notations:
         Cons A B == [A:B], [:] A B == [A:B].
         `Cons' is a built-in function taking its 2 arguments lazily.
	 In fact, [A:B] is translated to the application (Cons A B) by
	 the FUN parser. Miranda defines `:' as an infix operator.
*/

/* Length of list: (now available as built-in) */
/* letrec len = L l . if (null l) 0 (+ 1 (len (cdr l)));*/

/* i-th element of list; 0 is first: (now available as built-in) */
/*
letrec ith = L i l . if (null l) [] (if (== i 0) (car l) (ith (- i 1) (cdr l)));
*/

/* Returns list of legal subscript (ith) values for (possibly infinite) l: */
let index l = f 0 l where f n l = if (null l) [] [ n : f (+ n 1) (cdr l) ] end;

/* Return index of element e in list l: */
let pos l e = pos* l e 0
  where pos* l e p =
    if (null l) -1
      (if (== (car l) e) p
        (pos* (cdr l) e (+ 1 p)))
  end;

/* Enlist, i.e., put arg in singleton list: */
let enlist a = [a];

/* Concatenate two lists: (equivalent to the built-in `++') */
letrec append = L x y . if (null x) y [ car x : append (cdr x) y ];

/* Apply f to all elements of list:
   (Note: map f = foldr ([:].f) [])
*/
letrec map = L f l . if (null l) [] [ f (car l) : map f (cdr l) ];

/* Map a binary function over 2 lists; stops as soon as one of the lists
   is exhausted. Example: map2 + [1..4] [0..2] ==> [ 1, 3, 5 ]
*/
letrec map2 = L f l1 l2 . if (|| (null l1) (null l2)) []
                       [ f (car l1) (car l2) : map2 f (cdr l1) (cdr l2) ];

/* Not so useful:
letrec flatMap = L f l . if (null l) []
                            (++ (f (car l)) (flatMap f (cdr l)));
*/

/* Is x member of list l: */
letrec member = L x l . if (null l) False
			(|| (== (car l) x) (member x (cdr l)));

/* Equal lists (uses = on atoms): */
letrec equal = L l1 l2 . if (null l1) (null l2)
                        (if (null l2) False
                        (&& (== (car l1) (car l2)) (equal (cdr l1) (cdr l2))));

/* Reduce f over elements of l in a right-associative way: */
letrec foldr = L f def l . if (null l) def (f (car l) (foldr f def (cdr l)));

/* and, or :: [Bool] -> Bool */
let and = foldr (&&) True;
let or  = foldr (||) False;

/* any, all :: (a -> Bool) -> [a] -> Bool */
let any p = or  . map p;
let all p = and . map p;

/* Reduce f over elements of l from the left (in a left-associative way): */
/*
letrec foldl = L f def l . if (null l) def (foldl f (f def (car l)) (cdr l));
*/

/* foldl strict in 2-nd arg: */
let strict = S seq;
letrec foldl f def l = if (null l) def
                         (strict (foldl f) (f def (car l)) (cdr l));

/* Select elements satisfying predicate f from list l: */
letrec select = L f l . if (null l) []
                          (if (f (car l))
                             [ car l : select f (cdr l) ]
			   (select f (cdr l)));
/* Alternative:
let select f l = [ e | e <- l; f e ];
*/

/* Miranda name for select: */
let filter = select;

/* Take first i elements from list l: */
letrec take = L i l . if (|| (<= i 0) (null l)) []
		      [ car l : take (- i 1) (cdr l) ];

/* Take elements from list l while predicate p is True: */
letrec takewhile = L p l . if (null l) []
                          (if (p (car l))
  			     [ car l : takewhile p (cdr l) ]
			   []);

/* Drop first i elements off list l: */
letrec drop = L i l . if (|| (<= i 0) (null l)) l
                         (drop (- i 1) (cdr l));

/* Drop elements from list l while predicate p is True: */
letrec dropwhile = L p l . if (null l) []
                          (if (p (car l))
  			     (dropwhile p (cdr l))
			   l);

/* Apply f to x n times, i.e, (f^n) x; iter 0 f x ==> x. */
letrec iter n f = if (<= n 0) I ((iter (- n 1) f).f);

/* Generates infinite list of results of increasing repeated applications
   of f on x:
   [ x, f x, f (f x), f (f (f x)), ... ]
*/
letrec iterate f x = [ x : iterate f (f x) ];

/* Could also use cyclic structure:
let iterate f x = zs where zs = [ x : map f zs ] end;
*/

/* Remove first occurrence of x from list l: */
letrec remove l x = if (null l) []
  (let e = car l, r = cdr l in
     if (== x e) r [ e : remove r x ]);

/* List difference, for each element in `l2' its first occurrence in `l1'
   is removed from `l1':
*/
letrec -- l1 l2 = if (null l2) l1 ((remove l1 (car l2)) $-- (cdr l2));

/* Last element in a list: */
letrec last = L l . if (null (cdr l)) (car l) (last (cdr l));

/* Merge two sorted lists according ordering `<=' c: */
letrec merge = L c l1 l2 . if (null l1) l2
                          (if (null l2) l1
                          (if (c (car l1) (car l2))
                          [ car l1 : merge c (cdr l1) l2 ]
                          [ car l2 : merge c l1 (cdr l2) ]));

/* (Merge-)sort list according ordering `<=' c: */
letrec sort = L c l . if (< (len l) 2) l
                     (if (== (len l) 2)
			(let swap = L p . [ (car .cdr) p, car p ] in
                          (if (c (car l) ((car .cdr) l)) l (swap l)))
                     (merge c (sort c (take (/ (len l) 2) l))
                              (sort c (drop (/ (len l) 2) l))));

/* Reverse a list (note: using the C combinator): */
/* Note: can't use `foldr' */
let reverse = foldl (C Cons) [];

/* All permutations of the elements of a list x: */
letrec perms x = if (null x) [[]] [ [a:y] | a <- x; y <- perms (rm x a) ]
	           where rm x a = [ e | e <- x; != e a ] end;

/* Zips a pair of lists into a list of pairs (2-element lists): */
letrec zip = L l1 l2 . if (|| (null l1) (null l2)) []
                       [ [car l1 , car l2] : zip (cdr l1) (cdr l2) ];

/* Zips up as and bs by applying z to pairs of elements of as and bs and
   accumulating the results as a list until one of the lists is exhausted.
*/
letrec zipwith z as bs = if (|| (null as) (null bs)) []
                         [z (car as) (car bs) : zipwith z (cdr as) (cdr bs)];

letrec zipwith3 z as bs cs =
  if (|| (null as) (|| (null bs) (null cs))) []
  [z (car as) (car bs) (car cs) : zipwith3 z (cdr as) (cdr bs) (cdr cs)];

letrec zipwith4 z as bs cs ds =
  if (|| (null as) (|| (null bs) (|| (null cs) (null ds)))) []
  [z (car as) (car bs) (car cs) (car ds)
   : zipwith4 z (cdr as) (cdr bs) (cdr cs) (cdr ds)];

/* Removes adjacent duplicates from list, uses eq for equal test: */
letrec nodups eq l = if (|| (null l) (null (cdr l))) l
  (if (eq (car l) (car (cdr l))) (nodups eq (cdr l))
  [ car l : nodups eq (cdr l) ]);

/* Returns list with elements being the counts of adjacent duplicates;
   uses eq for equal test.
   Ex. cdups = [1,1,2,2,2,3,4,4] ==> [ 2, 3, 1, 2]
*/
let cdups eq l = if (null l) [] (cdups1 eq 1 l)
  where cdups1 eq n l = if (null (cdr l)) [ n ]
                    (if (eq (car l) (car (cdr l))) (cdups1 eq (+ n 1) (cdr l))
                     [ n : cdups1 eq 1 (cdr l) ])
  end;

/* Generate list of n replicas of x, e.g. rep 3 1 ==> [ 1, 1, 1 ] */
let rep n x = [ x | dummy <- [1..n] ];

/* Like rep but generates infinite list: */
letrec repeat x = [ x : repeat x ];

/* NUMBERs */

let INT_MIN = if (== (<< 1 32) 1) (<< 1 31) (<< 1 63);
let INT_MAX = >> -1 1;

/* The natural numbers as ascending list [ 0,1,2,.. ]: */
/* let N = letrec G = L n . [ n : G (+ n 1)] in G 0; */
/* Much conciser definition: (dot-dot notation of lists) */
let N = [0..];

/* These also work for characters: */
let succ = L n . (+ n 1);
let pred = L n . (- n 1);

/* Even predicate (also works for negative numbers): */
let even = L n . (== n (<< (>> n 1) 1));

/* Odd predicate: */
let odd = not . even;

/* Aliases: */
let div = /;
let mod = %;

/* Square: */
let sqr = L n . * n n;

/* Explicit unary minus (note: use of currying): */
let neg = - 0;

let min a b = if (< a b) a b;
let max a b = if (> a b) a b;

/* Sign (note: 3-valued): */
let sign = L n . if (< n 0) -1 (if (> n 0) 1 0);

/* Factorial: */
letrec fac = L n . if (> n 0) (* n (fac (- n 1))) 1;

/* All the factorials of N as list: */
let facs = map fac [0..];
/* Alternative:
let facs = letrec f k n = [ n : f (+ k 1) (* k n) ] in f 1 1;
*/

/* All the Fibonacci numbers as a list [ 0,1,1,2,3,5,8,..]: */
let fibs = letrec f = L a b . [ a : f b (+ a b) ] in f 0 1;

/* Interesting alternative:
letrec fibs = [ 1, 1 : [+ (car x) ((car .cdr) x) | x <- zip fibs (cdr fibs) ]];
*/

/* Greatest common divisor: */
letrec gcd = L n m . if (== m 0) n (gcd m (% n m));

/* n to the power m; better use built-in operator ^
letrec pow = L n m . if (== m 0) 1
                       (if (== m 1) n
                          (* n (pow n (- m 1))));
*/

/* All 2-powers as list [ 1,2,4,8,16, ... ]: */
/*let powers_of_two = map (^ 2) [0..];*/
/* alternatively: [ ^ 2 n | n <- [0..]] */
/* Can only go upto 2^lg2(INT_MAX). */

/* The list of all prime numbers:
   (Using sieve of Eratosthenes)
*/
let primes = sieve [2..]
  where sieve l = [ car l : sieve [ y | y <- cdr l; (> (% y (car l)) 0) ]] end;

/* Primeness predicate: */
let primep n = mem n primes
  where mem n l = (|| (== n (car l)) (&& (> n (car l)) (mem n (cdr l)))),
        primes = sieve [2..]
    where sieve l = [ car l : sieve [ y | y <- cdr l; (> (% y (car l)) 0) ]]
    end
 end;

/* The list of all factors of n (including 1 and n itself): */
/*let factors n = ++ [ i | i <- [1../ n 2]; = (% n i) 0 ] [n];*/

/* The list of all factors of n (excluding n itself): */
let factors n = [ i | i <- [1..n $- 1]; n $% i $== 0 ];

/* The list of all prime factors of n: */
/*let pfactors n = [ i | i <- [2:[3,5../ n 2]]; = (% n i) 0; primep i ];*/

/* Takes product over all elements in a list: */
let product = foldr * 1;

/* Takes sum over all elements in a list: */
let sum = foldr + 0;

/* CHARs and STRINGs */

/* Strings are simply lists of CHARacters.
   The FUN parser translates a string to a character list, and the FUN printer
   does the opposite.
   Most built-in arithmetic functions also apply to CHARs.
*/

/* The ASCII character set: */
let ASCII = [ '\0'..'\177' ];
/* The list of all uppercase letters: */
let ABC = [ 'A'..'Z' ];
/* The list of all lowercase letters: */
let abc = [ 'a'..'z' ];

/* Predicates on characters: */
let isupper = L c . && (>= c 'A') (<= c 'Z');
let islower = L c . && (>= c 'a') (<= c 'z');
let isalpha = L c . || (isupper c) (islower c);
let isdigit = L c . && (>= c '0') (<= c '9');
let isspace = L c . member c " \t\n\r\v\f";

/* Upper/lower case conversion on a character: */
let toupper = L c . if (islower c) (- c 32) c;
let tolower = L c . if (isupper c) (+ c 32) c;

/* Upper/lower case conversion on a string: */
let   upcase = map toupper;
let downcase = map tolower;

/* ASCII value of char: */
let ord = + 0;

/* Integer as ASCII char: */
let chr = + '\0';

/* Lexicographic string comparison (3-valued): */ 
letrec strcmp = L s1 s2 . if (null s1) (if (null s2) 0 -1)
                         (if (null s2) 1
                         (if (< (car s1) (car s2)) -1
                         (if (== (car s1) (car s2)) (strcmp (cdr s1) (cdr s2))
                          1)));

/* Is string s member of list l? */
letrec strmem s l = if (null l) False
		   (|| (== (strcmp (car l) s) 0) (strmem s (cdr l)));

/* string <= */
/* don't like this name; let user define this for himself. */
/*letrec lexico_le = L s1 s2 . (<= (strcmp s1 s2) 0);*/

/* Split string at '\n' characters to form list of lines: */
letrec lines = L s . if (null s) []
                     let r = lines (cdr s) in
                       if (== (car s) '\n') [ [] : r ]
                       [ [ car s : car r ] : cdr r ];

/* Split string at isspace characters to form list of words: */
letrec words =
  L s . if (null s) []
    letrec skipw = L s . let ns = cdr s in
      if (null ns) [] (if (isspace (car ns)) (skipw ns) ns) in
        if (isspace (car s))
          (let ns = skipw s in
            if (null ns) [] [ [] : words ns ])
        let r = words (cdr s) in [ [ car s : car r ] : cdr r ];

/* Concatenates the lists that are elements of l: */
let concat = foldr ++ "";

/* Concatenates strings in list l with '\n' terminators added:
   (This is useful to undo the action of `lines').
*/
let lay = concat . (map (C ++ "\n"));
