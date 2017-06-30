FUN
===

## Experimental Functional Programming Language (version 1.3, Jan 1996)

*fun* is the name of a simple functional programming language with fully lazy
semantics and also the name of the program that reads a piece of source text
in the language and evaluates it. The language is based on lambda-expressions,
although thanks to the available extensive syntax most of the peculiarities of
lambda-expressions are hidden from the user.

*fun* mimics the functionality found in mature functional programming languages like
Miranda (TM), Haskell, and Gofer. Don't get confused: all built-in functions in
*fun* are defined as prefix operators.
*fun*
does however support a crude facility for infix expressions: any prefix
operator or user defined function may be enclosed in back-quotes (`) and as
such used inbetween operands. Instead of back-quotes, a single dollar-sign in
front of the operator name may be used as an alternative. All such infix
operators, unlike the built-in ones,
will assume the same (lowest) precedence and group from
left-to-right (= left-associative), unless, of course, this default behaviour
is explicitly overridden with the use of parentheses.

Missing in
*fun*
are function definitions by pattern-matching and any notion of modularisation,
like modules or abstract data types. Also,
*fun*
is weakly typed and type-checking is done at run-time, e.g. lists may
contain elements of any type. In contrast, Miranda requires lists to have
elements of a single type, and supplies the fixed-sized tuples data type to
handle record/struct-like data. For now, tuples may be emulated by lists and
uninterpreted names can be used as tags.
