/* Inspired by the book:
   "Miranda (TM) The Craft of Functional Programming"
   Simon Thomson,
   Addison-Wesley, 1995

   This file: Copyright (c) G. Janssen 1995
*/

/* Conventions for naming arguments:
   c : a character
   s : a string
   l : a list
   p : a predicate on characters,
       a parser
   t : a parse (sub-)tree
*/

/* Parse tree data structure.
   Mind that the names Bool, Char, Num, Str, and Comp are used as uninterpreted
   symbols, i.e., they reduce to themselves.
   Because we can test them with '=', they are used to tag the list structures
   that represent parse tree nodes.
*/
/* Used by mkComp to simplify the parse tree: */
let mkVoid   = [ ];
/* The basic leaf node: */
let mkChar c = [ Char : c ];
/* Internal nodes: */
/* If either l or r is Void (meaning accept immediately without gobbling up
   any chars from the input, then the other results.
*/
let mkComp l r = if (null l) r (if (null r) l [ Comp : [ l : r ] ]);

/* Lexers; recognizing a single char. */
let spot  p s = if (&& (! (null s)) (p (car s))) [[mkChar (car s),cdr s]] [];
let token c   = spot (== c);
let anychar   = spot (K True);
let anybut c  = spot (!= c);
let oneof s   = spot (C member s);
let noneof s  = spot (! . (C member s));

/* Elementary parsers: */
let fail    s    = [];			/* Accepts nothing */
let succeed t s  = [[ t , s ]];	/* Accepts immediately with parse tree t */
let endofinput s = if (null s) (succeed mkVoid s) (fail s);

/* Parsers for grouping symbols: */
let lparen   = token '(';
let rparen   = token ')';
let lbracket = token '[';
let rbracket = token ']';
let lbrace   = token '{';
let rbrace   = token '}';

/* Parsers for punctuation symbols: */
let colon     = token ':';
let semicolon = token ';';
let comma     = token ',';
let period    = token '.';
let dquote    = token '"';

/* Parsers for a letter, digit, white-space, and sign: */
let letter  = spot isalpha;
let digit   = spot isdigit;
let blank   = spot isspace;
let sgn     = oneof "+-";

/* Combinators of parsers: */
let alt  p1 p2 s = p1 s $++ p2 s;
/* Force longest match! */
let opt      p s = let r = p s in if (null r) (succeed mkVoid s) r;
/* Generates all alternatives: */
/*let opt      p   = succeed mkVoid $alt p;*/
let then p1 p2 s =
  [ [ mkComp (car r1) (car r2), car (cdr r2) ] |
    r1 <- p1 s;              not (null r1);
    r2 <- p2 (car (cdr r1)); not (null r2) ];
letrec oneormore  p = p $then opt (oneormore p);
let    zeroormore p = opt (oneormore p);

/* Convenient short-hand notation for parser functionals: */
let \| = alt;		/* Use as $\| for infix */
let \? = opt;		/* Prefix */
let \@ = then;		/* Use as $\@ for infix */
let \+ = oneormore;	/* Prefix */
let \* = zeroormore;	/* Prefix */
let \$ = endofinput;

/* Parser actions: */
/* Apply `f' to parse trees resulting from parser `p' on input `s': */
let do p f s = [ [ f (car r), car (cdr r) ] | r <- p s; not (null r) ];
/* Useful second arg to `do': */
let ignore = K mkVoid;
/* Recognize `p' and skip it, i.e., don't build any parse tree (mkVoid): */
let skip p = do p ignore;

/* Some more interesting parse tree leaf nodes: */
let mkBool b = [ Bool : c ];
let mkNum  n = [ Num  : n ];
let mkStr  s = [ Str  : s ];
let mkId   s = [ Id   : s ];
let mkOp   o = [ Op   : o ];

/* Convert parse tree to single Str cell: */
letrec mk_str t = if (null t) (mkStr "")
  (let type = car t, r = cdr t in
    if (== type Char) (mkStr [ r ])
   (if (== type Str) r
    /* else must be Comp type */
   (mkStr ((cdr (mk_str (car r))) $++ (cdr (mk_str (cdr r)))))));

/* Converts parse tree to Num cell: */
let mk_num t = let ds = cdr (mk_str t) in
  if (== (car ds) '+') (mkNum (mk_num1 0 (cdr ds)))
 (if (== (car ds) '-') (mkNum (* -1 (mk_num1 0 (cdr ds))))
 (mkNum (mk_num1 0 ds)))
  where mk_num1 n s = if (null s) n
    (mk_num1 (+ (* n 10) (- (car s) '0')) (cdr s)) end;

/* Converts parse tree to Id cell: */
let mk_id = mkId . cdr . mk_str;

let mk_op t = mkOp (let op = cdr t in
  if (== op '+') +
 (if (== op '-') -
 (if (== op '*') *
 (if (== op '/') /
  []))));

/* Useful multiple character parsers: */
let digits    = \+ digit;
let letters   = \+ letter;
let alphanums = \+ (letter $alt digit);
let blanks    = \+ blank;

/* Sample Grammar: (can't have left-recursive rules) */

/* Parsers with some semantic action: */
let blank* = \* blank $do ignore;
let    num = blank* $\@ \? sgn $\@ digits $do mk_num;
let  ident = blank* $\@ letter $\@ \? alphanums $do mk_id;
let string =
  blank* $\@ skip dquote $\@ \* (anybut '\"') $\@ skip dquote $do mk_str;
let   oper = blank* $\@ oneof "+-*/" $do mk_op;
let   lpar = blank* $\@ lparen $do ignore;
let   rpar = blank* $\@ rparen $do ignore;

letrec expr = num
          $\| ident
          $\| string
          $\| (lpar $\@ expr $\@ oper $\@ expr $\@ rpar)
          $\| (lpar $\@ expr $\@ rpar);

/* Parse input `s' as expr and return parse tree: */
let parse s = car (car ((expr $\@ \$) s));
