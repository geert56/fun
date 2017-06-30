data Bool = False | True;
/*
Char
Int
*/
type String = [Char];
data List a = Nil | Cons a (List a);
type List a = [a];

data Tree a = Leaf a | Branch (Tree a) (Tree a);
/*
a: generic or schematic type variable
Leaf, Branch: constructors of the type, may be used as functions
Tree: type-forming operator
*/
