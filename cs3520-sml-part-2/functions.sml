exception Undefined

(* Define a function member of type ''a * ''a list -> bool so that member (e,L)
   is true if and only if e is an element of the list L *)

fun member (x,nil) = false
	| member (x, y::ys) = x = y orelse member (x, ys)

(* Define a function less of type int * int list -> int list so that less (e,L)
   is a list of all the integers in L that are less than e *)

fun less (e, nil) = nil
   | less (e, x::xs) = if x < e then x :: less(e,xs) else less (e, xs);

(* Define a function repeats of type ''a list -> bool so that repeats L is true
   if and only if the list L has two equal elements next to each other *)

fun repeats (nil) = false
  | repeats (x::nil) = false
  | repeats (x::xs) = if x = hd(xs) then true else repeats(xs)

(* Represent a polynomial using a list of its (real) coefficients, starting with
   the constant coefficient and going only as high as necessary. For example,
   3x²+5x+1 would be represented as the list [1.0,5.0,3.0] and x³-2x as
   [0.0,~2.0,0.0,1.0]. Write a function eval of type real list * real -> real
   that takes a polynomial represented this way and a value for x and returns
   the value of that polynomial at the given x. For example,
   eval ([1.0,5.0,3.0],2.0) should evaluate to 23.0, because when x=2,
   3x²+5x+1=23 *)

fun eval ([]:real list, x:real) = 0.0
  | eval (L:real list, x:real) = hd(L) + x * eval (tl(L), x);

(* Write a bubblesort function of type int list -> int list. Here is a review of
   the bubblesort algorithm. Compare the first two elements of the list. If they
   are out of order relative to each other, swap them. Then compare the second
   and third elements and swap them if they are out of order, etc. After
   completing the list, repeat until you make a complete pass over the list with
   no swaps. Note: write any helper functions as local functions using
   let..in..end *)

fun sort [] = []
  | sort [x] = [x]
  | sort (x::y::xs) = if( y < x ) then y::sort(x::xs)
                      else x ::sort(y::xs)
fun bubblesort [] = []
  | bubblesort ( x :: xs ) = sort(x::(bubblesort(xs)))

(* In the following exercises, implement sets as lists, where each element of a
   set appears exactly once in the list and the elements appear in no particular
   order. Do not assume you can sort the lists. Do assume that input lists have
   no duplicate elements, and do guarantee that output lists have no duplicate
   elements. *)

(* Write a function to construct the union of two sets. It should have type
   ''a list * ''a list -> ''a list. Note: you may use the member function you
   defined earlier. *)

fun union ([], y) = y
 |  union(a::x, y) =
      if member(a,y) then union (x,y)
      else a::union(x,y);

(* Write a function to construct the intersection of two sets. It should have
   type ''a list * ''a list -> ''a list. *)

fun intersection ([], y) = []
   | intersection (a::x, y) = 
      if member(a,y) then a::intersection(x,y)
      else intersection(x,y)

(* Write a function to construct the powerset of any set. A set's powerset is
   the set of all of its subsets. Consider the set A={1,2,3}. It has various
   subsets: {1}, {1,2}, and so on. Of course the empty set, ∅, is a subset of
   every set. The powerset of A is the set of all subsets of A:

      {x: x ⊆ A}={∅, {1}, {2}, {3}, {1,2}, {1,3}, {2,3}, {1,2,3}}

   Your powerset function should take a list (representing the set) and return a
   list of lists (representing the set of all subsets of the original set).
   powerset [1,2] should return [[1,2], [1], [3], []] (in any order). Your
   powerset function need on work on the untyped empty list; it may give an
   error message when evaluating powerset nil. But it should work on a typed
   empty list, so powerset (nil : int list) should give the right answer ([[]]).
   *)

fun insert(a,[]) = []
 |   insert(a,b::y) = union([a],b)::insert(a,y);

fun powerset ([]) = [[]]
 | powerset(a::y) = union(powerset(y), insert(a,powerset(y)))
