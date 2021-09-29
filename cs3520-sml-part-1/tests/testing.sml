(* adapted from https://github.com/kvalle/sml-testing *)

use "tests/asserts.sml";

signature TESTS =
sig
    val test : string * string -> string
    val run : unit -> string
    val xmlout : string -> unit
end

structure SmlTests :> TESTS =
struct

val suite : (string * string) list ref = ref [];

fun test (desc, result) =
    (suite := (desc, result)::(!suite);
    desc)

fun print_errors tests =
    case tests of
	[] => ()
      | (description,result)::rest =>
	(print("FAILED: " ^ description ^ "\n  -- " ^ result ^ "\n");
	 print_errors(rest))

fun run () =
    let
	val failed = List.filter (fn (_,result) => result <> TEST_PASSED) (rev (!suite))
	val return = Int.toString(length (!suite)) ^ " tests total"
    in
	case failed of
	    [] => (print("\nTESTS PASSED\n\n");
		   return)
	  | _ => (print("\n");
		  print_errors(failed);
		  print("\n" ^ Int.toString(length(failed)) ^" test(s) failed\n\n");
		  return)
    end

fun xmlout filename =
    let
        val total = length (!suite)
        val passed = length (List.filter (fn (_,result) => result = TEST_PASSED) (!suite))
        val failed = total - passed
        val fp = TextIO.openOut filename
        fun report (description, result) = (
            let val exp = map str (explode description)
                val esc = map (fn "&" => "&amp;" | "'" => "&#39;" | "\"" => "&#34;" | "<" => "&lt;" | ">" => "&gt;" | s => s) exp
                val desc = foldr op^ "" esc
            in
              TextIO.output (fp, "    <testcase name=\""^desc^"\" status=\"run\" time=\"0\" classname=\"\"")
            end;
            if result = TEST_PASSED
                then TextIO.output (fp, " />\n")
                else TextIO.output (fp, ">\n      <failure><![CDATA["^result^"]]></failure>\n    </testcase>\n"))
        fun reportAll [] = ()
          | reportAll (x::xs) = (report x ; reportAll xs)
    in
        TextIO.output (fp, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        TextIO.output (fp, "<testsuites name=\"AllTests\" tests=\""^Int.toString(total)^
                           "\" failures=\""^Int.toString(failed)^
                           "\" disabled=\"0\" errors=\"0\">\n");
        TextIO.output (fp, "  <testsuite name=\"AllTests\" tests=\""^Int.toString(total)^
                           "\" failures=\""^Int.toString(failed)^
                           "\" disabled=\"0\" errors=\"0\" time=\"0\">\n");
        reportAll (rev (!suite));
        TextIO.output (fp, "  </testsuite>\n");
        TextIO.output (fp, "</testsuites>\n");
        TextIO.closeOut fp
    end
	
end
