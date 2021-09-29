(* basic parser combinators *)
exception Noparse

infixr 5 ++
infixr 3 >>
infixr 0 ||

fun (p1 ++ p2) input =
    let val (x, rest)  = p1 input
        val (y, rest') = p2 rest
    in ((x, y), rest') end

fun (p >> f) input =
    let val (x, rest) = p input
    in (f x, rest) end

fun (p1 || p2) input =
    p1 input
    handle Noparse => p2 input

fun many p input =
    let val (x, rest) = p input
        val (xs, rest') = many p rest
    in (x::xs, rest') end
    handle Noparse => ([], input)

fun some p [] = raise Noparse
  | some p (x::xs) = if p x then (x, xs)
                     else raise Noparse

fun a tok = some (fn item => item = tok)


(* parser for simple language *)

(* an integer is all digits or - followed by all digits *)
fun isInt s =
    let fun digits lst = List.all Char.isDigit lst
        val lst = explode s
    in
        digits lst orelse
        length lst > 1 andalso hd lst = #"-" andalso digits (tl lst)
    end

fun parseInput tokens =
    SOME ((parseExpression >> Expression || parseFunDef >> FunDef) tokens)
    handle Noparse => NONE

and parseFunDef tokens =
   (a "(" ++ a "define" ++ parseFunction ++ parseArgList ++ parseExpression ++ a ")"
        >> (fn (_, (_, (name, (formals, (body, _))))) => (name, formals, body))) tokens

and parseArgList tokens =
   (a "(" ++ many parseVariable ++ a ")"
        >> (fn (_, (args, _)) => args)) tokens

and parseExpression tokens =
       (parseValue
            >> ValExp
    ||  parseVariable
            >> VarExp
    ||  a "(" ++ a "if" ++ parseExpression ++ parseExpression ++ parseExpression ++ a ")"
            >> (fn (_, (_, (cond, (thenPart, (elsePart, _))))) => IfExp (cond, thenPart, elsePart))
    ||  a "(" ++ a "while" ++ parseExpression ++ parseExpression ++ a ")"
            >> (fn (_, (_, (cond, (body, _)))) => WhileExp (cond, body))
    ||  a "(" ++ a "set" ++ parseVariable ++ parseExpression ++ a ")"
            >> (fn (_, (_, (variable, (exp, _)))) => SetExp (variable, exp))
    ||  a "(" ++ a "begin" ++ parseExpression ++ many parseExpression ++ a ")"
            >> (fn (_, (_, (first, (rest, _)))) => BeginExp (first::rest))
    ||  a "(" ++ parseOp ++ many parseExpression ++ a ")"
            >> (fn (_, (name, (args, _))) =>
                    if isBinaryBuiltIn name then (case args of [left, right] => BinaryBuiltinExp (name, left, right) | _ => raise Noparse) else
                    if isUnaryBuiltIn  name then (case args of [arg]         => UnaryBuiltinExp  (name, arg)         | _ => raise Noparse) else
                    ApExp(name, args))) tokens

and parseOp tokens =
    (parseFunction || parseValueOp) tokens

and parseValue tokens =
    (parseInteger || parseQuotedConst) tokens

and parseValueOp tokens =
    some isBuiltIn tokens

and parseQuotedConst tokens =
    (a "'" ++ parseSExpression
        >> (fn (_, sxp) => sxp)) tokens

and parseSExpression tokens =
       (parseInteger
    ||  parseSymbol
            >> SymSxp
    ||  a "(" ++ many parseSExpression ++ a ")"
            >> (fn (_, (slist, _)) => foldr ListSxp NilSxp slist)) tokens

and parseSymbol tokens =
    parseName tokens

and parseFunction tokens =
    (parseName
        >> (fn s => if isReservedWord s orelse isBuiltIn s
                    then raise Noparse
                    else s)) tokens

and parseVariable tokens =
    parseName tokens

and parseInteger tokens =
    (some isInt
        >> (fn s => case Int.fromString s of
                      SOME n => NumSxp n
                    | NONE => raise Noparse)) tokens

and parseName tokens =
    some (fn x =>
            not (isInt x) andalso
            not (Char.contains x #"(" orelse
                 Char.contains x #")" orelse
                 Char.contains x #";" orelse
                 Char.contains x #" ")) tokens
