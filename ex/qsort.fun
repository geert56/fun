/* Tiggr's version: */
letrec quicksort1 = L cmp ls .
	if (null ls) []
	let x = car ls in
	let xs = cdr ls in
		++ (++ (quicksort1 cmp [y | y <- xs; cmp y x]) [x])
		(quicksort1 cmp [y | y <- xs; not (cmp y x)]);

let quicksort = quicksort1 <;

/* Geert's version: */
letrec qsort1 = L lt l .
	if (|| (null l) (null (cdr l))) l
	let  x = car l in
	let xs = cdr l in
          ++ (++ (qsort1 lt [ y | y <- xs; lt y x ])
                 [x])
	         (qsort1 lt [ y | y <- xs; not (lt y x) ]);

let qsort = qsort1 <;
