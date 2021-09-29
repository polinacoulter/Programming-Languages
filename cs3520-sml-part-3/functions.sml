exception Not_implemented

(* These exercises all have one-line solutions using map, foldr, or
   foldl. You can also use other predefined functions, but do NOT
   write any additional named functions and do NOT use explicit
   recursion. If you need helper functions, use anonymous ones. For
   example, if the problem says "write a function add2 that takes an
   int list and returns the same list with 2 added to every
   element", your answer should be

      fun add2 x = map (fn a => a + 2) x

   You have seen some of these problems before. The trick now is to
   solve them in this new, concise form. *)

(* Write a function il2rl of type int list -> real list that takes a
   list of integers and returns a list of the same numbers converted
   to type real. For example, if you evaluate il2rl [1,2,3] you
   should get [1.0,2.0,3.0]. *)

fun il2rl x = map real x

(* Write a function ordlist of type char list -> int list that takes
   a list of characters and returns the list of the integer codes of
   those characters. For example, if you evaluate ordlist
   [#"A",#"b",#"C"] you should get [65,98,67]. *)

fun ordlist x = map ord x

(* Write a function squarelist of type int list -> int list that
   takes a list of integers and returns a list of the squares of
   those integers. For example, if you evaluate squarelist [1,2,3,4]
   you should get [1,4,9,16]. *)

fun squarelist (l) = map (fn x => x * x) l

(* Write a function multpairs of type (int * int) list -> int list
   that takes a list of pairs of integers and returns a list of the
   products of each pair. For example, if the input is
   [(1,2), (3,4)], your function should return [2,12]. *)

fun multpairs (l) = map (op *) l

(* Write a function inclist of type int list -> int -> int list that
   takes a list of integers and an integer increment, and returns
   the same list of integers but with the integer increment added to
   each one. For example, if you evaluate inclist [1,2,3,4] 10 you
   should get [11,12,13,14]. Note that the function is curried. *)

fun inclist list inc = map (fn x => x + inc) list

(* Write a function sqsum of type int list -> int that takes a list
   of integers and returns the sum of the squares of those integers.
   For example, if you evalute sqsum [1,2,3,4] you should get 30. *)

fun sqsum x = foldr (op +) 0 (map (fn x => x * x) x)

(* Write a function bor of type bool list -> bool that takes a list
   of boolean values and returns the logical OR of all of them. If
   the list is empty, your function should return false. *)

fun bor x = foldr ( fn(a,b) => a orelse b) false x

(* Write a function band of type bool list -> bool that takes a list
   of boolean values and returns the logical AND of all of them. If
   the list is empty, your function should return true. *)

fun band x = let fun & (a,b) = a andalso b in foldr (op &) true x end;

(* Write a function bxor of type bool list -> bool that takes a list
   of boolean values and returns the logical exclusive OR of all of
   them. (It should return true if the number of true values in the
   list is odd and false if the number of true values is even.) If
   the list is empty, your function should return false. *)

fun bxor x = foldr ( fn (x,y) => not (y) andalso x orelse( y andalso not (x) ) ) false x

(* Write a function duplist of type 'a list -> 'a list whose output
   list is the same as the input list, but with each element of the
   input list repeated twice in a row. For example, if the input
   list is [1,3,2], the output list should be [1,1,3,3,2,2]. If the
   input list is [], the output list should be []. *)

fun duplist l = foldr ( fn (a,b) => a::a::b) nil l

(* Write a function mylength of type 'a list -> int that returns the
   length of a list. (Of course, you may not use the predefined
   length function to do it. *)

fun mylength l = foldr ( fn(x,length) => length + 1) 0 l

(* Write a function il2absrl of type int list -> real list that
   takes a list of integers and returns a list containing the
   absolute value of those integers, converted to real numbers. *)

fun il2absrl u = map (fn x => abs (real x) ) u

(* Write a function truecount of type bool list -> int that takes a
   list of boolean values and returns the number of trues in the
   list. *)

fun truecount l = foldr ( fn(x,length) => if x = true then length + 1 else length) 0 l

(* Write a function maxpairs of type (int * int) list -> int list
   that takes a list of pairs of integers and returns the list of
   the max elements from each pair. For example, if you evaluate
   maxpairs [(1,3), (4,2), (~3,~4)] you should get [3,4,~3]. *)

fun maxpairs l = map ( fn(a,b) => if a > b then a else b) l

(* Write a function myimplode that works just like the predefined
   implode. In other words, it should be a function of type
   char list -> string that takes a list of characters and returns
   the string containing those same characters in the same order. *)

fun myimplode l = foldr (op ^) "" (map str l)

(* Write a function lconcat of type 'a list list -> 'a list that
   takes a list of lists as input and returns the list formed by
   appending the input lists together in order. For example, if the
   input is [[1,2], [3,4,5,6], [7]], your function should return
   [1,2,3,4,5,6,7]. (There is a predefined function like this called
   concat, which of course you should not use.) *)

fun lconcat l = foldr ( fn(a,b) => a @ b ) [] l

(* Write a function max of type int list -> int that returns the
   largest element of a list of integers. Your function need not
   behave well if the list is empty. *)

fun max l = foldr ( fn(x,high) => if x > high then x else high) 0 l

(* Write a function min of type int list -> int that returns the
   smallest element of a list of integers. Your function need not
   behave well if the list is empty. *)

fun min l = foldr ( fn(x,low) => if x < low then x else low) 999999999 l

(* Write a function member of type ''a * ''a list -> bool so that
   member (e,L) is true if and only if e is an element of list L. *)

fun member (e,l) = foldl ( fn(item,a) => (a) orelse (e = item) ) false l

(* Write a function append of type 'a list -> 'a list -> 'a list
   that takes two lists and returns the result of appending the
   second one onto the end of the first. For example, append [1,2,3]
   [4,5,6] should evaluate to [1,2,3,4,5,6]. Do not use the
   predefined appending utilities, like the @ operator or the
   contact function. Note that the function is curried. *)

fun append lst1 lst2 = foldr (op ::) lst2 lst1

(* Define a function less of type int * int list -> int list so that
   less (e,L) is a list of all the integers in L that are less than
   e (in any order). *)

fun less (e,nil) = nil
	| less (e, x::xs) = if x < e then x :: less(e,xs) else less(e,xs)

(* Write a function evens of type int list -> int list that takes a
   list of integers and returns the list of all the even elements
   from the original list (in the original order). For example, if
   you evaluate evens [1,2,3,4] you should get [2,4]. *)

fun evens [] = []
	| evens(x::xs) = if x mod 2 = 0 then x::evens(xs) else evens(xs)

(* Write a function convert of type ('a * 'b) list -> 'a list * 'b * list,
   that converts a list of pairs into a pair of lists, preserving
   the order of the elements. For example, convert [(1,2), (3,4),
   (5,6)] should evaluate to ([1,3,5], [2,4,6]). *)

fun convert l = foldl ( fn( (x,y),(u,v)) => (u@[x], v@[y]) ) (nil,nil) l

(* Define a function mymap with the same type and behavior as map,
   but without using map. (Note this should still be a one-liner:
   use foldl or foldr.) *)

fun mymap input nil = nil
	| mymap input (p::q) = input p::mymap input q


(* Represent a polynomial using a list of its (real) coefficients,
   starting with the constant coefficient and going only as high as
   necessary. For example, 3x²+5x+1 would be represented as the list
   [1.0,5.0,3.0] and x³-2x as [0.0,~2.0,0.0,1.0]. Write a function
   eval of type real list -> real -> real that takes a polynomial
   represented this way and a value for x and returns the value of
   that polynomial at the given x. For example, eval
   [1.0,5.0,3.0] 2.0 should evaluate to 23.0, because when x=2,
   3x²+5x+1=23. (This is the same as the problem from the previous
   problem set, except that now it is a curried function and must be
   written as a one liner.) *)

fun eval ys x = foldr ( fn(y,a) => y + x*a) 0.0 ys
