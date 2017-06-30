/* Binary trees:  tree * :: Nil | Tree * (tree *) (tree *)
   The trees are simply represented as lists.
   Copyright (c) 1995 G. Janssen
*/

/* Tree component selectors: */
let NodeVal tree = (car)         tree;
let Left    tree = (car.cdr)     tree;
let Right   tree = (car.cdr.cdr) tree;

/* Tree constructor functions: */
let  Nil       = [];
let Tree n l r = [ n, l, r ];
let Node n     = Tree n Nil Nil;

/* Tree node predicates: */
let  Nilp      = null;
let Leafp tree = && (Nilp (Left tree)) (Nilp (Right tree));

/* Gen.Purp. tree traversal.
   Takes 3 functions + tree as arguments.
   Arg 1: called for each Nil tree,
   Arg 2: called for each NodeVal
   Arg 3: called for each non-Nil tree
*/
letrec traverse Nilf NodeValf Treef t =
  if (Nilp t) (Nilf t)
    (Treef (NodeValf (NodeVal t))
      (traverse Nilf NodeValf Treef (Left t))
      (traverse Nilf NodeValf Treef (Right t)));

/* Applies f to all NodeVals of a tree. */
let tmap f = traverse id f Tree;

/* Reduces f (takes 3-args) over tree t. d is unit-element of f. */
letrec tfoldpre f d t =
  if (Nilp t) d
    (f (NodeVal t) (tfoldpre f d (Left t)) (tfoldpre f d (Right t)));

/* Preorder list of NodeVals: */
let  preorder = traverse id enlist L v l r . ++ v (++ l r);
/* Or, equivalently:
let  preorder = tfoldpre (L a b c . [ a : ++ b c ]) [];
*/

let   inorder = traverse id enlist L v l r . ++ l (++ v r);
let postorder = traverse id enlist L v l r . ++ (++ l r) v;

/* Exchange left and right subtrees in all non-leaf nodes: */
let reflect = traverse id id L v l r . Tree v r l;

/* NodeVals of leaf nodes from left to right as a list: */
letrec fringe t = if (Nilp t) []
                    (if (Leafp t) [ NodeVal t ]
                    (++ (fringe (Left t)) (fringe (Right t))));

letrec nrnodes t = if (Nilp t) 0
                     (+ 1 (+ (nrnodes (Left t)) (nrnodes (Right t))));

letrec height t = if (Nilp t) 0
                    (+ 1 (max (height (Left t)) (height (Right t))));

/* Constructs complete (fully balanced) binary tree with n levels;
   0 levels returns Nil tree.
   NodeVals will be consecutive numbers per level, starting at 0 for the root.

let complete_tree n =
  letrec ctree n m =
    if (<= n 0) Nil
      let k = - n 1, l = * 2 m in
        Tree m (ctree k (+ l 1)) (ctree k (+ l 2))
  in ctree n 0;

  Below: identical function, but now using `where-clauses'.
*/
let complete_tree n = ctree n 0
  where ctree n m =
    if (<= n 0) Nil (Tree m (ctree k (+ l 1)) (ctree k (+ l 2)))
    where k = - n 1, l = * 2 m end
  end;

let showtree shownode t =
  letrec prlev t l sp =
   (letrec spaces sp = rep sp ' ' in
    if (Nilp t) (spaces sp)
      let nsp = / sp 2, lsp = (- nsp 1), rsp = - (- sp nsp) 1 in
        if (== l 1) (++ (++ (spaces lsp)
                           (shownode (NodeVal t)))
                           (spaces rsp))
        (++ (prlev (Left  t) (- l 1) nsp)
            (prlev (Right t) (- l 1) nsp))) in
  letrec prlevs h l t = if (<= l h) (++ (++ (prlev t l 80) "\n")
                                            (prlevs h (+ l 1) t))
                          "" in
    prlevs (height t) 1 t;

/* This one uses where-clauses.

let showtree shownode t = prlevs (height t) 1 t
  where prlevs h l t = if (<= l h)
                         (++ (++ (prlev t l 80) "\n") (prlevs h (+ l 1) t))
                         ""
    where prlev t l sp =
      if (Nilp t) (spaces sp)
        (if (== l 1)
          (++ (++ (spaces lsp) (shownode (NodeVal t))) (spaces rsp))
         (++ (prlev (Left  t) (- l 1) nsp) (prlev (Right t) (- l 1) nsp)))
      where spaces sp = rep sp ' ',
            nsp       = / sp 2,
            lsp       = - nsp 1,
            rsp       = - (- sp nsp) 1
      end
    end
  end;
*/

/* Add a string to a binary search tree. */
letrec addString s t = if (Nilp t) (Node s)
  let n = (NodeVal t), cmp = strcmp s n in
    if (< cmp 0) (Tree n (addString s (Left  t)) (Right t))
   (if (> cmp 0) (Tree n (Left t) (addString s (Right t)))
    t);

letrec addStringL l t = if (null l) t
  (addStringL (cdr l) (addString (car l) t));
