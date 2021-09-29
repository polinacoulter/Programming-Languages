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

(*test("member (5,[]) is false", assert_false(member (5,[]))) handle _ => * test("member (5,[]) is false", "runtime error");*)
test("il2rl [] is []", assert_equals_real_list([], il2rl [])) handle _ => test("il2rl [] is []", "runtime error");
test("il2rl [~1,7,1234] is [~1.0,7.0,1234.0]", assert_equals_real_list([~1.0,7.0,1234.0], il2rl [~1,7,1234])) handle _ => test("il2rl [~1,7,1234] is [~1.0,7.0,1234.0]", "runtime error");
test("il2rl [1,2,3,4,5] is [1.0,2.0,3.0,4.0,5.0]", assert_equals_real_list([1.0,2.0,3.0,4.0,5.0], il2rl [1,2,3,4,5])) handle _ => test("il2rl [1,2,3,4,5] is [1.0,2.0,3.0,4.0,5.0]", "runtime error");
test("ordlist [] is []", assert_equals_int_list([], ordlist [])) handle _ => test("ordlist [] is []", "runtime error");
test("ordlist [A,b,C] is [65,98,67]", assert_equals_int_list([65,98,67], ordlist [#"A",#"b",#"C"])) handle _ => test("ordlist [A,b,C] is [65,98,67]", "runtime error");
test("ordlist [3,L,E,3,t] is [51,76,69,51,116]", assert_equals_int_list([51,76,69,51,116], ordlist [#"3",#"L",#"E",#"3",#"t"])) handle _ => test("ordlist [3,L,E,3,t] is [51,76,69,51,116]", "runtime error");
test("squarelist [] is []", assert_equals_int_list([], squarelist [])) handle _ => test("squarelist [] is []", "runtime error");
test("squarelist [1,2,3,4] is [1,4,9,16]", assert_equals_int_list([1,4,9,16], squarelist [1,2,3,4])) handle _ => test("squarelist [1,2,3,4] is [1,4,9,16]", "runtime error");
test("squarelist [0,~3,16,~256,15] is [0,9,256,65536,225]", assert_equals_int_list([0,9,256,65536,225], squarelist [0,~3,16,~256,15])) handle _ => test("squarelist [0,~3,16,~256,15] is [0,9,256,65536,225]", "runtime error");
test("multpairs [] is []", assert_equals_int_list([], multpairs [])) handle _ => test("multpairs [] is []", "runtime error");
test("multpairs [(1,2), (3,4)] is [2,12]", assert_equals_int_list([2,12], multpairs [(1,2), (3,4)])) handle _ => test("multpairs [(1,2), (3,4)] is [2,12]", "runtime error");
test("multpairs [(~6,3), (7,~4), (0,345), (4,4)] is [~18,~28,0,16]", assert_equals_int_list([~18,~28,0,16], multpairs [(~6,3), (7,~4), (0,345), (4,4)])) handle _ => test("multpairs [(~6,3), (7,~4), (0,345), (4,4)] is [~18,~28,0,16]", "runtime error");
test("inclist [] 0 is []", assert_equals_int_list([], inclist [] 0)) handle _ => test("inclist [] 0 is []", "runtime error");
test("inclist [1,2,3,4] 10 is [11,12,13,14]", assert_equals_int_list([11,12,13,14], inclist [1,2,3,4] 10)) handle _ => test("inclist [1,2,3,4] 10 is [11,12,13,14]", "runtime error");
test("inclist [5,4,3,6,7,8] ~4 is [1,0,~1,2,3,4]", assert_equals_int_list([1,0,~1,2,3,4], inclist [5,4,3,6,7,8] ~4)) handle _ => test("inclist [5,4,3,6,7,8] ~4 is [1,0,~1,2,3,4]", "runtime error");
test("sqsum [] is 0", assert_equals_int(0, sqsum [])) handle _ => test("sqsum [] is 0", "runtime error");
test("sqsum [1,2,3,4] is 30", assert_equals_int(30, sqsum [1,2,3,4])) handle _ => test("sqsum [1,2,3,4] is 30", "runtime error");
test("sqsum [0,~1,2,~3,4,~5] is 55", assert_equals_int(55, sqsum [0,~1,2,~3,4,~5])) handle _ => test("sqsum [0,~1,2,~3,4,~5] is 55", "runtime error");
test("bor [] is false", assert_false(bor [])) handle _ => test("bor [] is false", "runtime error");
test("bor [false,false,false,false,false,false] is false", assert_false(bor [false,false,false,false,false,false])) handle _ => test("bor [false,false,false,false,false,false] is false", "runtime error");
test("bor [false,false,false,false,true,false] is true", assert_true(bor [false,false,false,false,true,false])) handle _ => test("bor [false,false,false,false,true,false] is true", "runtime error");
test("band [] is true", assert_true(band [])) handle _ => test("band [] is true", "runtime error");
test("band [true,true,true,true] is true", assert_true(band [true,true,true,true])) handle _ => test("band [true,true,true,true] is true", "runtime error");
test("band [true,false,true,true] is false", assert_false(band [true,false,true,true])) handle _ => test("band [true,false,true,true] is false", "runtime error");
test("bxor [] is false", assert_false(bxor [])) handle _ => test("bxor [] is false", "runtime error");
test("bxor [true,false,true,true,false] is true", assert_true(bxor [true,false,true,true,false])) handle _ => test("bxor [true,false,true,true,false] is true", "runtime error");
test("bxor [true,false,true,true,false,true] is false", assert_false(bxor [true,false,true,true,false,true])) handle _ => test("bxor [true,false,true,true,false,true] is false", "runtime error");
test("duplist [] is []", assert_equals_int_list([], duplist [])) handle _ => test("duplist [] is []", "runtime error");
test("duplist [1,3,2] is [1,1,3,3,2,2]", assert_equals_int_list([1,1,3,3,2,2], duplist [1,3,2])) handle _ => test("duplist [1,3,2] is [1,1,3,3,2,2]", "runtime error");
test("duplist [u,v,x,y,z] is [u,u,v,v,x,x,y,y,z,z]", assert_equals_string_list(["u","u","v","v","x","x","y","y","z","z"], duplist ["u","v","x","y","z"])) handle _ => test("duplist [u,v,x,y,z] is [u,u,v,v,x,x,y,y,z,z]", "runtime error");
test("mylength [] is 0", assert_equals_int(0, mylength [])) handle _ => test("mylength [] is 0", "runtime error");
test("mylength [8,3,5,6,6,3,1,0] is 8", assert_equals_int(8, mylength [8,3,5,6,6,3,1,0])) handle _ => test("mylength [8,3,5,6,6,3,1,0] is 8", "runtime error");
test("mylength [a,b,c,d,e,f,g] is 7", assert_equals_int(7, mylength ["a","b","c","d","e","f","g"])) handle _ => test("mylength [a,b,c,d,e,f,g] is 7", "runtime error");
test("il2absrl [1,~2,3,~4,6,~7] is [1.0,2.0,3.0,4.0,6.0,7.0]", assert_equals_real_list([1.0,2.0,3.0,4.0,6.0,7.0], il2absrl [1,~2,3,~4,6,~7])) handle _ => test("il2absrl [1,~2,3,~4,6,~7] is [1.0,2.0,3.0,4.0,6.0,7.0]", "runtime error");
test("il2absrl [~1,2,~3,4,~6,7] is [1.0,2.0,3.0,4.0,6.0,7.0]", assert_equals_real_list([1.0,2.0,3.0,4.0,6.0,7.0], il2absrl [~1,2,~3,4,~6,7])) handle _ => test("il2absrl [~1,2,~3,4,~6,7] is [1.0,2.0,3.0,4.0,6.0,7.0]", "runtime error");
test("il2absrl [3,5,7,9,~3,~5,~7] is [3.0,5.0,7.0,9.0,3.0,5.0,7.0]", assert_equals_real_list([3.0,5.0,7.0,9.0,3.0,5.0,7.0], il2absrl [3,5,7,9,~3,~5,~7])) handle _ => test("il2absrl [3,5,7,9,~3,~5,~7] is [3.0,5.0,7.0,9.0,3.0,5.0,7.0]", "runtime error");
test("truecount [true,true,false,true,false,true,false,false] is 4", assert_equals_int(4, truecount [true,true,false,true,false,true,false,false])) handle _ => test("truecount [true,true,false,true,false,true,false,false] is 4", "runtime error");
test("truecount [true,true,false,true,true,true,false,false,true] is 6", assert_equals_int(6, truecount [true,true,false,true,true,true,false,false,true])) handle _ => test("truecount [true,true,false,true,true,true,false,false,true] is 6", "runtime error");
test("truecount [false,false,true,false,false,false,true,true,false] is 3", assert_equals_int(3, truecount [false,false,true,false,false,false,true,true,false])) handle _ => test("truecount [false,false,true,false,false,false,true,true,false] is 3", "runtime error");
test("maxpairs [(1,3), (4,2), (~3,~4)] is [3,4,~3]", assert_equals_int_list([3,4,~3], maxpairs [(1,3), (4,2), (~3,~4)])) handle _ => test("maxpairs [(1,3), (4,2), (~3,~4)] is [3,4,~3]", "runtime error");
test("maxpairs [(3,1), (2,4), (~4,~3)] is [3,4,~3]", assert_equals_int_list([3,4,~3], maxpairs [(3,1), (2,4), (~4,~3)])) handle _ => test("maxpairs [(3,1), (2,4), (~4,~3)] is [3,4,~3]", "runtime error");
test("maxpairs [(0,4), (5,5), (6,7), (10,9)] is [4,5,7,10]", assert_equals_int_list([4,5,7,10], maxpairs [(0,4), (5,5), (6,7), (10,9)])) handle _ => test("maxpairs [(0,4), (5,5), (6,7), (10,9)] is [4,5,7,10]", "runtime error");
test("myimplode [] is \"\"", assert_equals_string("", myimplode [])) handle _ => test("myimplode [] is \"\"", "runtime error");
test("myimplode [a,b,c,d,e] is \"abcde\"", assert_equals_string("abcde", myimplode [#"a",#"b",#"c",#"d",#"e"])) handle _ => test("myimplode [a,b,c,d,e] is \"abcde\"", "runtime error");
test("myimplode [h,i, ,w,o,r,l,d] is \"hi world\"", assert_equals_string("hi world", myimplode [#"h",#"i",#" ",#"w",#"o",#"r",#"l",#"d"])) handle _ => test("myimplode [h,i, ,w,o,r,l,d] is \"hi world\"", "runtime error");
test("lconcat [[1,2], [3,4,5,6], [7]] is [1,2,3,4,5,6,7]", assert_equals_int_list([1,2,3,4,5,6,7], lconcat [[1,2], [3,4,5,6], [7]])) handle _ => test("lconcat [[1,2], [3,4,5,6], [7]] is [1,2,3,4,5,6,7]", "runtime error");
test("lconcat [[q,r], [s,t], [u,v,w,x,y], [z]] is [q,r,s,t,u,v,w,x,y,z]", assert_equals_string_list(["q","r","s","t","u","v","w","x","y","z"], lconcat [["q","r"], ["s","t"], ["u","v","w","x","y"], ["z"]])) handle _ => test("lconcat [[q,r], [s,t], [u,v,w,x,y], [z]] is [q,r,s,t,u,v,w,x,y,z]", "runtime error");
test("lconcat [[], [9], [8], [], [7], [6,5,4,3,2,1], []] is [9,8,7,6,5,4,3,2,1]", assert_equals_int_list([9,8,7,6,5,4,3,2,1], lconcat [[], [9], [8], [], [7], [6,5,4,3,2,1], []])) handle _ => test("lconcat [[], [9], [8], [], [7], [6,5,4,3,2,1], []] is [9,8,7,6,5,4,3,2,1]", "runtime error");
test("max [9,6,2,3,5,5,1] is 9", assert_equals_int(9, max [9,6,2,3,5,5,1])) handle _ => test("max [9,6,2,3,5,5,1] is 9", "runtime error");
test("max [9,6,2,3,5,5,10] is 10", assert_equals_int(10, max [9,6,2,3,5,5,10])) handle _ => test("max [9,6,2,3,5,5,10] is 10", "runtime error");
test("max [9,6,2,35,5,10] is 35", assert_equals_int(35, max [9,6,2,35,5,10])) handle _ => test("max [9,6,2,35,5,10] is 35", "runtime error");
test("min [9,6,2,3,5,5,1] is 1", assert_equals_int(1, min [9,6,2,3,5,5,1])) handle _ => test("min [9,6,2,3,5,5,1] is 1", "runtime error");
test("min [2,6,2,3,5,5,10] is 2", assert_equals_int(2, min [2,6,2,3,5,5,10])) handle _ => test("min [2,6,2,3,5,5,10] is 2", "runtime error");
test("min [9,6,2,~4,5,10] is ~4", assert_equals_int(~4, min [9,6,2,~4,5,10])) handle _ => test("min [9,6,2,~4,5,10] is ~4", "runtime error");
test("member (2, [7,1,3,4,2]) is true", assert_true(member (2, [7,1,3,4,2]))) handle _ => test("member (2, [7,1,3,4,2]) is true", "runtime error");
test("member (j, [a,s,d,f,j,k]) is true", assert_true(member (#"j", [#"a",#"s",#"d",#"f",#"j",#"k"]))) handle _ => test("member (j, [a,s,d,f,j,k]) is true", "runtime error");
test("member (j, [a,s,d,f,l,k]) is false", assert_false(member (#"j", [#"a",#"s",#"d",#"f",#"l",#"k"]))) handle _ => test("member (j, [a,s,d,f,l,k]) is false", "runtime error");
test("append [1,2,3] [4,5,6] is [1,2,3,4,5,6]", assert_equals_int_list([1,2,3,4,5,6], append [1,2,3] [4,5,6])) handle _ => test("append [1,2,3] [4,5,6] is [1,2,3,4,5,6]", "runtime error");
test("append [a,b,c,d] [] is [a,b,c,d]", assert_equals_string_list(["a","b","c","d"], append ["a","b","c","d"] [])) handle _ => test("append [a,b,c,d] [] is [a,b,c,d]", "runtime error");
test("append [e,f] [g,h,i] is [e,f,g,h,i]", assert_equals_string_list(["e","f","g","h","i"], append ["e","f"] ["g","h","i"])) handle _ => test("append [e,f] [g,h,i] is [e,f,g,h,i]", "runtime error");
test("less (5, [2,3,4,5,6,7]) is [2,3,4]", assert_equals_int_list([2,3,4], sort (less (5, [2,3,4,5,6,7])))) handle _ => test("less (5, [2,3,4,5,6,7]) is [2,3,4]", "runtime error");
test("less (8, [10,11,8,7,6]) is [6,7]", assert_equals_int_list([6,7], sort (less (8, [10,11,8,7,6])))) handle _ => test("less (8, [10,11,8,7,6]) is [6,7]", "runtime error");
test("less (0, [0,1,2,3,4]) is []", assert_equals_int_list([], less (0, [0,1,2,3,4]))) handle _ => test("less (0, [0,1,2,3,4]) is []", "runtime error");
test("evens [1,2,3,4] is [2,4]", assert_equals_int_list([2,4], evens [1,2,3,4])) handle _ => test("evens [1,2,3,4] is [2,4]", "runtime error");
test("evens [3,4,5,~3,~4,~5,6,0] is [4,~4,6,0]", assert_equals_int_list([4,~4,6,0], evens [3,4,5,~3,~4,~5,6,0])) handle _ => test("evens [3,4,5,~3,~4,~5,6,0] is [4,~4,6,0]", "runtime error");
test("evens [3,5,~3,~7,~5,1,13] is []", assert_equals_int_list([], evens [3,5,~3,~7,~5,1,13])) handle _ => test("evens [3,5,~3,~7,~5,1,13] is []", "runtime error");
test("convert [] is ([], [])", let val (a,b)=convert [] in assert_equals_int_list([], a); assert_equals_int_list([], b) end) handle _ => test("convert [] is ([], [])", "runtime error");
test("convert [(1,2), (3,4), (5,6), (7,8)] is ([1,3,5,7], [2,4,6,8])", let val (a,b)=convert [(1,2), (3,4), (5,6), (7,8)] in assert_equals_int_list([1,3,5,7], a); assert_equals_int_list([2,4,6,8], b) end) handle _ => test("convert [(1,2), (3,4), (5,6), (7,8)] is ([1,3,5,7], [2,4,6,8])", "runtime error");
test("convert [(a,b), (c,d), (e,f)] is ([a,c,e], [b,d,f])", let val (a,b)=convert [("a","b"), ("c","d"), ("e","f")] in assert_equals_string_list(["a","c","e"], a); assert_equals_string_list(["b","d","f"], b) end) handle _ => test("convert [(a,b), (c,d), (e,f)] is ([a,c,e], [b,d,f])", "runtime error");
test("mymap (fn x => x+1) [1,2,3,4] is [2,3,4,5]", assert_equals_int_list([2,3,4,5], mymap (fn x => x+1) [1,2,3,4])) handle _ => test("mymap (fn x => x+1) [1,2,3,4] is [2,3,4,5]", "runtime error");
test("mymap (fn x => x*x) [5,4,3,2,1] is [25,16,9,4,1]", assert_equals_int_list([25,16,9,4,1], mymap (fn x => x*x) [5,4,3,2,1])) handle _ => test("mymap (fn x => x*x) [5,4,3,2,1] is [25,16,9,4,1]", "runtime error");
test("mymap (fn s => s^s) [a,b,c,d] is [aa,bb,cc,dd]", assert_equals_string_list(["aa","bb","cc","dd"], mymap (fn s => s^s) ["a","b","c","d"])) handle _ => test("mymap (fn s => s^s) [a,b,c,d] is [aa,bb,cc,dd]", "runtime error");
test("eval [] 7.0 is 0.0", assert_equals_real(0.0, eval [] 7.0)) handle _ => test("eval [] 7.0 is 0.0", "runtime error");
test("eval [1.0,5.0] 2.0 is 11.0", assert_equals_real(11.0, eval [1.0,5.0] 2.0)) handle _ => test("eval [1.0,5.0] 2.0 is 11.0", "runtime error");
test("eval [1.0,5.0,3.0] 2.0 is 23.0", assert_equals_real(23.0, eval [1.0,5.0,3.0] 2.0)) handle _ => test("eval [1.0,5.0,3.0] 2.0 is 23.0", "runtime error");

fun main() =
    (run();
    xmlout "test_detail.xml");
