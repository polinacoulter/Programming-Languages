use "ast.sml";
use "parse.sml";
use "eval.sml";

(*
 * The tokenizer
 *)

(* get a single token, reading new input lines (and printing prompts) as necessary *)
fun get stream prompt line [] =
        (* read a line if our buffer is empty *)
        let val _ = TextIO.output (TextIO.stdOut, prompt)
            val _ = TextIO.flushOut TextIO.stdOut
        in case TextIO.inputLine stream of
             SOME text => get stream prompt (line+1) (explode text)
           | NONE => NONE
        end

  (* skip whitespace *)
  | get stream prompt line (#" "::xs)  = get stream prompt line xs
  | get stream prompt line (#"\t"::xs) = get stream prompt line xs

  (* newline is returned as a token so the prompt can be changed *)
  | get stream prompt line (#"\n"::xs) = SOME ("\n", line, xs)

  (* comments skip to end of line *)
  | get stream prompt line (#";"::xs)  = SOME ("\n", line, [])

  (* special tokens *)
  | get stream prompt line (#"("::xs)  = SOME ("(",  line, xs)
  | get stream prompt line (#")"::xs)  = SOME (")",  line, xs)
  | get stream prompt line (#"'"::xs)  = SOME ("'",  line, xs)

  (* scan until the next special token or whitespace *)
  | get stream prompt line lst =
        let fun f (rest as (x::xs)) token =
                if Char.contains "(); \t\n" x then (token, rest)
                else f xs (token^(str x))
              | f [] token = (token, [])
            val (token, rest) = f lst ""
        in SOME (token, line, rest) end

(* read a list of tokens until end of line with balanced parentheses *)
fun readOne stream line =
    let fun f pcount prompt line rest lst =
                case get stream prompt line rest of
                (* if get reaches end-of-stream then we give up *)
                  NONE => NONE

                (* newline ends it if we have tokens and parentheses are balanced (or negative) *)
                | SOME ("\n", line1, _) =>
                    if pcount <= 0 andalso not (null lst)
                        then SOME (line1, rev lst)
                        else f pcount (if null lst then prompt else "  > ") line1 [] lst

                (* count parentheses *)
                | SOME ("(", line1, rest1) =>
                    f (pcount+1) prompt line1 rest1 ("("::lst)
                | SOME (")", line1, rest1) =>
                    f (pcount-1) prompt line1 rest1 (")"::lst)

                (* keep reading tokens *)
                | SOME (token, line1, rest1) =>
                    f pcount prompt line1 rest1 (token::lst)

    (* start with the default prompt *)
    in f 0 "--> " line [] [] end

(*
 * The read-eval-print loop
 *)
fun repl source =
    let fun f isFile stream line =
            case readOne stream line of NONE =>
                    (* when we reach the end of a file, switch over to stdin *)
                    if isFile
                        then (TextIO.closeIn stream; print "\n"; f false TextIO.stdIn 0)
                        else print "\n"
            | SOME (_, ["quit"]) =>
                    (if isFile
                        then TextIO.closeIn stream else (); print "\n")
            | SOME (line1, tokens) =>
                    (* parse the input *)
                    (case parseInput tokens of NONE =>
                        (* parse error *)
                        (TextIO.output (TextIO.stdErr, "Syntax error on input that started on line "^(intToString line)^"\n");
                         TextIO.flushOut TextIO.stdErr;
                         f isFile stream line1)

                    | SOME (_, token::_) =>
                        (* there was junk left over after the parser finished *)
                        (TextIO.output (TextIO.stdErr, "Found extra token beyond end of input: "^token^"\n");
                         TextIO.flushOut TextIO.stdErr;
                         f isFile stream line1)

                    | SOME (FunDef (def as (name, _, _)), _) =>
                        (* function definition found--add it to the global function list *)
                        (functionSet def; print (name^"\n");
                         f isFile stream line1)

                    | SOME (Expression exp, _) =>
                        (* expression found--evaluate it and print the result *)
                        ((case eval ([], exp) of (_, result) => print ((sxpToString result)^"\n"))
                        handle (RuntimeError msg) =>
                            (TextIO.output (TextIO.stdErr, msg^" (on input that started on line "^(intToString line)^")\n");
                             TextIO.flushOut TextIO.stdErr);
                        f isFile stream line1))
    in
        f (source <> "") (if source = "" then TextIO.stdIn else TextIO.openIn source) 0
    end

fun main () =
   (print "Lisp interpreter\n";
    print "Fall 2019 version\n";
    case CommandLine.arguments () of
      [] => repl ""
    | [inputFile] => repl inputFile
    | _ => (TextIO.output (TextIO.stdErr, "Usage: "^(CommandLine.name ())^" [<inputfile>]\n");
            TextIO.flushOut TextIO.stdErr;
            OS.Process.exit OS.Process.failure))
