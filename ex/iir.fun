/* Infinite Impulse Response Filter */

let h X = [ 0 : X ];

/* Adding 4 numbers: */
let plus4 a b c d = + (+ (+ a b) c) d;

/* Adding 4 streams of numbers: */
letrec S A B C D = zipwith4 plus4 A B C D;

/* Multiplying a stream X by a factor a: */
letrec mul a X = map (* a) X;

/* Y1 Pulse => [ 0, 1, 1, 2, 3, 5, ... ] (fibs) */
let a0 = 1,
    a1 = 1,
    a2 = 0;
/*
let a0 = 0.5,
    a1 = 0.3,
    a2 = 0.1;
*/
letrec Y0 In = S In (mul a0 (Z0 In)) (mul a1 (Z1 In)) (mul a2 (Z2 In)),
       Y1 In = h (Y0 In),
       Y2 In = h (Y1 In),
       Y3 In = h (Y2 In),
       Z0 = Y1,
       Z1 = Y2,
       Z2 = Y3,
       Out In = Y3 In;

let Pulse = [1:[0,0..]];
