use "tests/testing.sml";
use "functions.sml";
open SmlTests;

test("0 cubed is 0", assert_equals_int(0, cube 0)) handle _ => test("0 cubed is 0", "runtime error");
test("1 cubed is 1", assert_equals_int(1, cube 1)) handle _ => test("1 cubed is 1", "runtime error");
test("2 cubed is 8", assert_equals_int(8, cube 2)) handle _ => test("2 cubed is 8", "runtime error");
test("5 cubed is 125", assert_equals_int(125, cube 5)) handle _ => test("5 cubed is 125", "runtime error");
test("0.0 cubed is 0.0", assert_equals_real(0.0, cuber 0.0)) handle _ => test("0.0 cubed is 0.0", "runtime error");
test("1.5 cubed is 3.375", assert_equals_real(3.375, cuber 1.5)) handle _ => test("1.5 cubed is 3.375", "runtime error");
test("3.1416 cubed is 31.006494", assert_equals_real(31.006494, cuber 3.1416)) handle _ => test("3.1416 cubed is 31.006494", "runtime error");
test("4th elt of [1,2,3,4,5] is 4", assert_equals_int(4, fourth [1,2,3,4,5])) handle _ => test("4th elt of [1,2,3,4,5] is 4", "runtime error");
test("4th elt of [1.0,2.0,3.0,4.0,5.0] is 4.0", assert_equals_real(4.0, fourth [1.0,2.0,3.0,4.0,5.0])) handle _ => test("4th elt of [1.0,2.0,3.0,4.0,5.0] is 4.0", "runtime error");
test("4th elt of [a,b,c,d,e,f] is d", assert_equals_string("d", fourth ["a","b","c","d","e","f"])) handle _ => test("4th elt of [a,b,c,d,e,f] is d", "runtime error");
test("min3 (0,0,0) is 0", assert_equals_int(0, min3 (0,0,0))) handle _ => test("min3 (0,0,0) is 0", "runtime error");
test("min3 (0,1,~2) is ~2", assert_equals_int(~2, min3 (0,1,~2))) handle _ => test("min3 (0,1,~2) is ~2", "runtime error");
test("min3 (4,6,8) is 4", assert_equals_int(4, min3 (4,6,8))) handle _ => test("min3 (4,6,8) is 4", "runtime error");
test("min3 (9,2,6) is 2", assert_equals_int(2, min3 (9,2,6))) handle _ => test("min3 (9,2,6) is 2", "runtime error");
test("min3 (~3,~7,~9) is ~9", assert_equals_int(~9, min3 (~3,~7,~9))) handle _ => test("min3 (~3,~7,~9) is ~9", "runtime error");
test("red3 (1,2,3) is (1,3)", assert_equals_any((1,3), red3 (1,2,3))) handle _ => test("red3 (1,2,3) is (1,3)", "runtime error");
test("red3 (a,b,c) is (a,c)", assert_equals_any(("a","c"), red3 ("a","b","c"))) handle _ => test("red3 (a,b,c) is (a,c)", "runtime error");
test("red3 (true,false,true) is (true,true)", assert_equals_any((true,true), red3 (true,false,true))) handle _ => test("red3 (true,false,true) is (true,true)", "runtime error");
test("thirds hello is l", assert_equals_any(#"l", thirds "hello")) handle _ => test("thirds hello is l", "runtime error");
test("thirds goodbye is o", assert_equals_any(#"o", thirds "goodbye")) handle _ => test("thirds goodbye is o", "runtime error");
test("cycle1 [1,2,3,4] is [2,3,4,1]", assert_equals_int_list([2,3,4,1], cycle1 [1,2,3,4])) handle _ => test("cycle1 [1,2,3,4] is [2,3,4,1]", "runtime error");
test("cycle1 [a,b,c,d] is [b,c,d,a]", assert_equals_string_list(["b","c","d","a"], cycle1 ["a","b","c","d"])) handle _ => test("cycle1 [a,b,c,d] is [b,c,d,a]", "runtime error");
test("cycle1 [7,6,5,4,3,2,1] is [6,5,4,3,2,1,7]", assert_equals_int_list([6,5,4,3,2,1,7], cycle1 [7,6,5,4,3,2,1])) handle _ => test("cycle1 [7,6,5,4,3,2,1] is [6,5,4,3,2,1,7]", "runtime error");
test("sort3 (1.0,2.0,3.0) is [1.0,2.0,3.0]", assert_equals_real_list([1.0,2.0,3.0], sort3 (1.0,2.0,3.0))) handle _ => test("sort3 (1.0,2.0,3.0) is [1.0,2.0,3.0]", "runtime error");
test("sort3 (2.0,3.0,1.0) is [1.0,2.0,3.0]", assert_equals_real_list([1.0,2.0,3.0], sort3 (2.0,3.0,1.0))) handle _ => test("sort3 (2.0,3.0,1.0) is [1.0,2.0,3.0]", "runtime error");
test("sort3 (3.0,2.0,1.0) is [1.0,2.0,3.0]", assert_equals_real_list([1.0,2.0,3.0], sort3 (3.0,2.0,1.0))) handle _ => test("sort3 (3.0,2.0,1.0) is [1.0,2.0,3.0]", "runtime error");
test("sort3 (2.0,6.0,4.0) is [2.0,4.0,6.0]", assert_equals_real_list([2.0,4.0,6.0], sort3 (2.0,6.0,4.0))) handle _ => test("sort3 (2.0,6.0,4.0) is [2.0,4.0,6.0]", "runtime error");
test("sort3 (4.0,2.0,6.0) is [2.0,4.0,6.0]", assert_equals_real_list([2.0,4.0,6.0], sort3 (4.0,2.0,6.0))) handle _ => test("sort3 (4.0,2.0,6.0) is [2.0,4.0,6.0]", "runtime error");
test("sort3 (6.0,2.0,4.0) is [2.0,4.0,6.0]", assert_equals_real_list([2.0,4.0,6.0], sort3 (6.0,2.0,4.0))) handle _ => test("sort3 (6.0,2.0,4.0) is [2.0,4.0,6.0]", "runtime error");
test("sort3 (1.0,1.0,1.0) is [1.0,1.0,1.0]", assert_equals_real_list([1.0,1.0,1.0], sort3 (1.0,1.0,1.0))) handle _ => test("sort3 (1.0,1.0,1.0) is [1.0,1.0,1.0]", "runtime error");
test("del3 [1,2,3,4,5] is [1,2,4,5]", assert_equals_int_list([1,2,4,5], del3 [1,2,3,4,5])) handle _ => test("del3 [1,2,3,4,5] is [1,2,4,5]", "runtime error");
test("del3 [9,8,7,6,5,4,3,2,1] is [9,8,6,5,4,3,2,1]", assert_equals_int_list([9,8,6,5,4,3,2,1], del3 [9,8,7,6,5,4,3,2,1])) handle _ => test("del3 [9,8,7,6,5,4,3,2,1] is [9,8,6,5,4,3,2,1]", "runtime error");
test("del3 [1.1,2.2,3.3,4.4,5.5] is [1.1,2.2,4.4,5.5]", assert_equals_real_list([1.1,2.2,4.4,5.5], del3 [1.1,2.2,3.3,4.4,5.5])) handle _ => test("del3 [1.1,2.2,3.3,4.4,5.5] is [1.1,2.2,4.4,5.5]", "runtime error");
test("sqsum 0 is 0", assert_equals_int(0, sqsum 0)) handle _ => test("sqsum 0 is 0", "runtime error");
test("sqsum 5 is 55", assert_equals_int(55, sqsum 5)) handle _ => test("sqsum 5 is 55", "runtime error");
test("sqsum 10 is 385", assert_equals_int(385, sqsum 10)) handle _ => test("sqsum 10 is 385", "runtime error");
test("cycle ([1,2,3,4,5,6], 2) is [3,4,5,6,1,2]", assert_equals_int_list([3,4,5,6,1,2], cycle([1,2,3,4,5,6], 2))) handle _ => test("cycle ([1,2,3,4,5,6], 2) is [3,4,5,6,1,2]", "runtime error");
test("cycle ([a,b,c,d,e,f,g], 3) is [d,e,f,g,a,b,c]", assert_equals_string_list(["d","e","f","g","a","b","c"], cycle(["a","b","c","d","e","f","g"], 3))) handle _ => test("cycle ([a,b,c,d,e,f,g], 3) is [d,e,f,g,a,b,c]", "runtime error");
test("cycle ([a,b,c], 3) is [a,b,c]", assert_equals_string_list(["a","b","c"], cycle(["a","b","c"], 3))) handle _ => test("cycle ([a,b,c], 3) is [a,b,c]", "runtime error");
test("pow (0.5, 3) is 0.125", assert_equals_real(0.125, pow (0.5, 3))) handle _ => test("pow (0.5, 3) is 0.125", "runtime error");
test("pow (7.3, 5) is 20730.71593", assert_equals_real(20730.71593, pow (7.3, 5))) handle _ => test("pow (7.3, 5) is 20730.71593", "runtime error");
test("pow (3.14, 0) is 1.0", assert_equals_real(1.0, pow (3.14, 0))) handle _ => test("pow (3.14, 0) is 1.0", "runtime error");
test("max [1,3,9,7,2] is 9", assert_equals_int(9, max [1,3,9,7,2])) handle _ => test("max [1,3,9,7,2] is 9", "runtime error");
test("max [~1,~3,~9,~7,~2] is ~1", assert_equals_int(~1, max [~1,~3,~9,~7,~2])) handle _ => test("max [~1,~3,~9,~7,~2] is ~1", "runtime error");
test("max [0,1,2,2,1,1,0] is 2", assert_equals_int(2, max [0,1,2,2,1,1,0])) handle _ => test("max [0,1,2,2,1,1,0] is 2", "runtime error");
test("isPrime 1 is false", assert_false(isPrime 1)) handle _ => test("isPrime 1 is false", "runtime error");
test("isPrime 2 is true", assert_true(isPrime 2)) handle _ => test("isPrime 2 is true", "runtime error");
test("isPrime 3 is true", assert_true(isPrime 3)) handle _ => test("isPrime 3 is true", "runtime error");
test("isPrime 4 is false", assert_false(isPrime 4)) handle _ => test("isPrime 4 is false", "runtime error");
test("isPrime 5 is true", assert_true(isPrime 5)) handle _ => test("isPrime 5 is true", "runtime error");
test("isPrime 99 is false", assert_false(isPrime 99)) handle _ => test("isPrime 99 is false", "runtime error");
test("isPrime 100 is false", assert_false(isPrime 100)) handle _ => test("isPrime 100 is false", "runtime error");
test("isPrime 101 is true", assert_true(isPrime 101)) handle _ => test("isPrime 101 is true", "runtime error");
test("isPrime 102 is false", assert_false(isPrime 102)) handle _ => test("isPrime 102 is false", "runtime error");
test("isPrime 103 is true", assert_true(isPrime 103)) handle _ => test("isPrime 103 is true", "runtime error");
test("isPrime 104 is false", assert_false(isPrime 104)) handle _ => test("isPrime 104 is false", "runtime error");
test("isPrime 65535 is false", assert_false(isPrime 65535)) handle _ => test("isPrime 65535 is false", "runtime error");
test("isPrime 65537 is true", assert_true(isPrime 65537)) handle _ => test("isPrime 65537 is true", "runtime error");
fun sort lst =
    let fun f (n, lst as x::xs) =
                if n <= x then n::lst
                else x :: (f (n, xs))
          | f (n, _) = [n]
    in foldl f [] lst end;
test("select ([1,2,3,4,5], even) is [2,4]", assert_equals_int_list([2,4], sort (select ([1,2,3,4,5], fn x => x mod 2 = 0)))) handle _ => test("select ([1,2,3,4,5], even) is [2,4]", "runtime error");
test("select ([5,4,3,2,1], odd) is [1,3,5]", assert_equals_int_list([1,3,5], sort (select ([5,4,3,2,1], fn x => x mod 2 <> 0)))) handle _ => test("select ([5,4,3,2,1], odd) is [1,3,5]", "runtime error");
test("select ([1,3,5,7,9,11,13,15,17,19,21,23], isPrime) is [3,5,7,11,13,17,19,23]", assert_equals_int_list([3,5,7,11,13,17,19,23], sort (select ([1,3,5,7,9,11,13,15,17,19,21,23], isPrime)))) handle _ => test("select ([1,3,5,7,9,11,13,15,17,19,21,23], isPrime) is [3,5,7,11,13,17,19,23]", "runtime error");

fun main() =
    (run();
    xmlout "test_detail.xml");
