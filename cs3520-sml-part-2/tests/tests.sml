use "tests/testing.sml";
use "functions.sml";
open SmlTests;

fun sort lst =
    let fun f (n, lst as x::xs) =
                if n <= x then n::lst
                else x :: (f (n, xs))
          | f (n, _) = [n]
    in foldl f [] lst end;
fun sorts lst =
    let fun f (n:string, lst as x::xs) =
                if n <= x then n::lst
                else x :: (f (n, xs))
          | f (n, _) = [n]
    in foldl f [] lst end;
fun lst2str lst =
  let fun f [] = ""
        | f [x] = Int.toString x
        | f (x::xs) = Int.toString x ^ "," ^ f xs
  in "[" ^ f (sort lst) ^ "]" end;

test("member (5,[]) is false", assert_false(member (5,[]))) handle _ => test("member (5,[]) is false", "runtime error");
test("member (3,[1,2,3,4,5] is true", assert_true(member (3,[1,2,3,4,5]))) handle _ => test("member (3,[1,2,3,4,5] is true", "runtime error");
test("member (a,[a,b,c]) is true", assert_true(member ("a", ["a","b","c"]))) handle _ => test("member (a,[a,b,c]) is true", "runtime error");
test("member (z,[w,x,y,z]) is true", assert_true(member ("z", ["w","x","y","z"]))) handle _ => test("member (z,[w,x,y,z]) is true", "runtime error");
test("member (o,[i,j,k]) is false", assert_false(member ("o", ["i","j","k"]))) handle _ => test("member (o,[i,j,k]) is false", "runtime error");
test("less (5,[1,2,3,4,5]) is [1,2,3,4]", assert_equals_int_list([1,2,3,4], less (5,[1,2,3,4,5]))) handle _ => test("less (5,[1,2,3,4,5]) is [1,2,3,4]", "runtime error");
test("less (8,[7,9,6,10,5]) is [7,6,5]", assert_equals_int_list([7,6,5], less (8,[7,9,6,10,5]))) handle _ => test("less (8,[7,9,6,10,5]) is [7,6,5]", "runtime error");
test("less (4,[7,3,4,~4,9]) is [3,~4]", assert_equals_int_list([3,~4], less (4,[7,3,4,~4,9]))) handle _ => test("less (4,[7,3,4,~4,9]) is [3,~4]", "runtime error");
test("repeats [] is false", assert_false(repeats [])) handle _ => test("repeats [] is false", "runtime error");
test("repeats [1] is false", assert_false(repeats [0])) handle _ => test("repeats [1] is false", "runtime error");
test("repeats [3,4] is false", assert_false(repeats [3,4])) handle _ => test("repeats [3,4] is false", "runtime error");
test("repeats [1,2,3,3,4,5] is true", assert_true(repeats [1,2,3,3,4,5])) handle _ => test("repeats [1,2,3,3,4,5] is true", "runtime error");
test("repeats [1,2,3,4,4,5] is true", assert_true(repeats [1,2,3,4,4,5])) handle _ => test("repeats [1,2,3,4,4,5] is true", "runtime error");
test("repeats [1,2,3,4,5,5] is true", assert_true(repeats [1,2,3,4,5,5])) handle _ => test("repeats [1,2,3,4,5,5] is true", "runtime error");
test("repeats [1,2,3,4,5,6] is false", assert_false(repeats [1,2,3,4,5,6])) handle _ => test("repeats [1,2,3,4,5,6] is false", "runtime error");
test("eval ([],7.0) is 0.0", assert_equals_real(0.0, eval ([],7.0))) handle _ => test("eval ([],7.0) is 0.0", "runtime error");
test("eval ([1.0],2.0) is 1.0", assert_equals_real(1.0, eval ([1.0],2.0))) handle _ => test("eval ([1.0],2.0) is 1.0", "runtime error");
test("eval ([1.0,5.0],2.0) is 11.0", assert_equals_real(11.0, eval ([1.0,5.0],2.0))) handle _ => test("eval ([1.0,5.0],2.0) is 11.0", "runtime error");
test("eval ([1.0,5.0,3.0],2.0) is 23.0", assert_equals_real(23.0, eval ([1.0,5.0,3.0],2.0))) handle _ => test("eval ([1.0,5.0,3.0],2.0) is 23.0", "runtime error");
test("eval ([0.0,~2.0,0.0,1.0],4.0) is 56.0", assert_equals_real(56.0, eval ([0.0,~2.0,0.0,1.0],4.0))) handle _ => test("eval ([0.0,~2.0,0.0,1.0],4.0) is 56.0", "runtime error");
test("bubblesort [] is []", assert_equals_int_list([], bubblesort [])) handle _ => test("bubblesort [] is []", "runtime error");
test("bubblesort [9,8,7,6,5] is [5,6,7,8,9]", assert_equals_int_list([5,6,7,8,9], bubblesort [9,8,7,6,5])) handle _ => test("bubblesort [9,8,7,6,5] is [5,6,7,8,9]", "runtime error");
test("bubblesort [1,2,3,5,4] is [1,2,3,4,5]", assert_equals_int_list([1,2,3,4,5], bubblesort [1,2,3,5,4])) handle _ => test("bubblesort [1,2,3,5,4] is [1,2,3,4,5]", "runtime error");
test("bubblesort [5,6,4,7,3,8,2,9,1,1] is [1,1,2,3,4,5,6,7,8,9]", assert_equals_int_list([1,1,2,3,4,5,6,7,8,9], bubblesort [5,6,4,7,3,8,2,9,1,1])) handle _ => test("bubblesort [5,6,4,7,3,8,2,9,1,1] is [1,1,2,3,4,5,6,7,8,9]", "runtime error");
test("union ([1,2,3], [3,4,5]) is [1,2,3,4,5]", assert_equals_int_list([1,2,3,4,5], sort (union ([1,2,3], [3,4,5])))) handle _ => test("union ([1,2,3], [3,4,5]) is [1,2,3,4,5]", "runtime error");
test("union ([], [4,3]) is [4,3]", assert_equals_int_list([3,4], sort(union ([], [4,3])))) handle _ => test("union ([], [4,3]) is [4,3]", "runtime error");
test("union ([9,8,7,6], [5,6,7]) is [5,6,7,8,9]", assert_equals_int_list([5,6,7,8,9], sort(union ([9,8,7,6], [5,6,7])))) handle _ => test("union ([9,8,7,6], [5,6,7]) is [5,6,7,8,9]", "runtime error");
test("union ([a,d,b,c], [c,e,d]) is [a,b,c,d,e]", assert_equals_string_list(["a","b","c","d","e"], sorts(union (["a","d","b","c"], ["c","e","d"])))) handle _ => test("union ([a,d,b,c], [c,e,d]) is [a,b,c,d,e]", "runtime error");
test("intersection ([1,2,3], [3,4,5]) is [3]", assert_equals_int_list([3], sort (intersection ([1,2,3], [3,4,5])))) handle _ => test("intersection ([1,2,3], [3,4,5]) is [3]", "runtime error");
test("intersection ([], [4,3]) is []", assert_equals_int_list([], sort(intersection ([], [4,3])))) handle _ => test("intersection ([], [4,3]) is []", "runtime error");
test("intersection ([9,8,7,6], [5,6,7]) is [6,7]", assert_equals_int_list([6,7], sort(intersection ([9,8,7,6], [5,6,7])))) handle _ => test("intersection ([9,8,7,6], [5,6,7]) is [6,7]", "runtime error");
test("intersection ([a,d,b,c], [c,e,d]) is [c,d]", assert_equals_string_list(["c","d"], sorts(intersection (["a","d","b","c"], ["c","e","d"])))) handle _ => test("intersection ([a,d,b,c], [c,e,d]) is [c,d]", "runtime error");
test("powerset [] is [[]]", assert_equals_string_list(["[]"], sorts (map lst2str (powerset [])))) handle _ => test("powerset [] is [[]]", "runtime error");
test("powerset [5] is [[],[5]]", assert_equals_string_list(["[5]","[]"], sorts (map lst2str (powerset [5])))) handle _ => test("powerset [5] is [[],[5]]", "runtime error");
test("powerset [8,5] is [[],[5],[8],[8,5]]", assert_equals_string_list(["[5,8]","[5]","[8]","[]"], sorts (map lst2str (powerset [8,5])))) handle _ => test("powerset [8,5] is [[],[5],[8],[8,5]]", "runtime error");
test("powerset [8,5,9,1,3] is long", assert_equals_string_list(["[1,3,5,8,9]", "[1,3,5,8]", "[1,3,5,9]", "[1,3,5]", "[1,3,8,9]", "[1,3,8]", "[1,3,9]", "[1,3]", "[1,5,8,9]", "[1,5,8]", "[1,5,9]", "[1,5]", "[1,8,9]", "[1,8]", "[1,9]", "[1]", "[3,5,8,9]", "[3,5,8]", "[3,5,9]", "[3,5]", "[3,8,9]", "[3,8]", "[3,9]", "[3]", "[5,8,9]", "[5,8]", "[5,9]", "[5]", "[8,9]", "[8]", "[9]", "[]"], sorts (map lst2str (powerset [8,5,9,1,3])))) handle _ => test("powerset [8,5,9,1,3] is long", "runtime error");

fun main() =
    (run();
    xmlout "test_detail.xml");
