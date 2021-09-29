exception RuntimeError of string

local
    val globals: (string * sxp) list ref = ref []
    val functions: (string * (string list) * expression) list ref = ref []
in
    fun globalGet key =
        let fun f [] = NONE
              | f ((k,v)::tail) = if k = key then SOME v else f tail
        in f (!globals) end

    fun globalSet key value =
        let fun f [] = [(key, value)]
              | f ((k,v)::tail) = if k = key then (k,value)::tail else (k,v)::f tail
        in globals := f (!globals) end

    fun functionGet name =
        let fun f [] = NONE
              | f ((def as (k,_,_))::tail) = if k = name then SOME def else f tail
        in f (!functions) end

    fun functionSet (def as (name, _, _)) =
        let fun f [] = [def]
              | f ((elt as (k,_,_))::tail) = if k = name then def::tail else elt::f tail
        in functions := f (!functions) end

    fun rhoGet [] _ = NONE
      | rhoGet ((key, value)::tail) name =
            if key = name then SOME value else rhoGet tail name

    fun rhoSet [] key value = [(key, value)]
      | rhoSet ((elt as (k, v)) :: tail) key value =
            if key = k then (key, value) :: tail else elt :: rhoSet tail key value

    fun rhoContains rho name =
        case rhoGet rho name of SOME _ => true | NONE => false
end

(* your code goes here *)
(*fun eval (rho, exp) =
    (print (expressionToRepr exp ^ "\n");
     (rho, NilSxp))
*)

(* your code goes here *)
fun eval (rho, ValExp value) =
        (rho, value)
    
    | eval (rho, VarExp name) =
        (case rhoGet rho name of
            SOME value => (rho, value)
            | NONE => (case globalGet name of
                SOME value => (rho, value)
                | NONE => raise RuntimeError ("Undefined variable: "^name)))

    | eval (rho, SetExp (name, exp)) =
        let val (rho1, value) = eval (rho, exp)
        in if rhoContains rho1 name
            then (rhoSet rho1 name value, value)
            else (globalSet name value; (rho1, value))
        end

    | eval (rho, IfExp (exp, thenExp, elseExp)) =
        (case eval (rho, exp) of
            (rho1, NilSxp) => eval (rho1, elseExp)
            | (rho1, _) => eval (rho1, thenExp))

    | eval (rho, exp as WhileExp (condition, body)) =
        (case eval (rho, condition) of
            (rho1, NilSxp) => (rho1, NilSxp)
            |(rho1, _) => 
                (case eval (rho1, body) of (rho2, _) => eval (rho2, exp)))

    | eval (rho, BeginExp []) = raise RuntimeError "BeginExp with no body"
    | eval (rho, BeginExp [exp]) = eval (rho, exp)
    | eval (rho, BeginExp (exp::tail)) =
        (case eval (rho, exp) of
            (rho1, _) => eval (rho1, BeginExp tail))

    | eval (rho, BinaryBuiltinExp (name, left, right)) = 
        let val (rho1, leftValue) = eval (rho, left)
            val (rho2, rightValue) = eval (rho1, right)
        in case (name, leftValue, rightValue) of
              ("+", NumSxp leftValue, NumSxp rightValue) => (rho2, NumSxp (leftValue + rightValue))
            | ("-", NumSxp leftValue, NumSxp rightValue) => (rho2, NumSxp (leftValue - rightValue))
            | ("*", NumSxp leftValue, NumSxp rightValue) => (rho2, NumSxp (leftValue * rightValue))
            | ("/", NumSxp leftValue, NumSxp rightValue) => (rho2, NumSxp (leftValue div rightValue))
            | ("<", NumSxp leftValue, NumSxp rightValue) => (rho2, if leftValue < rightValue then SymSxp "T" else NilSxp) 
            | (">", NumSxp leftValue, NumSxp rightValue) => (rho2, if leftValue > rightValue then SymSxp "T" else NilSxp) 
            | ("=", NumSxp leftValue, NumSxp rightValue) => (rho2, if leftValue = rightValue then SymSxp "T" else NilSxp) 
            | ("=", SymSxp leftValue, SymSxp rightValue) => (rho2, if leftValue = rightValue then SymSxp "T" else NilSxp) 
            | ("=", NilSxp, NilSxp) => (rho2, SymSxp "T")
            | ("=", _, _) => (rho2, NilSxp)
            (*| ("cons", x, ListSxp y) => (rho2, ListSxp(x,y)) *)
            | ("cons", x, NilSxp) => (rho2, ListSxp(x, NilSxp))
            | ("cons", x, y) => (rho2, ListSxp(x, y))
            | (_, _, _) => raise RuntimeError ("Invalid Expression: "^name)
        end

    | eval (rho, UnaryBuiltinExp (name, exp)) = 
        (case (name, eval (rho, exp)) of
            ("print", (rho1, value)) => (print ((sxpToString value)^"\n"); (rho1, value))
            | ("car", (rho1, ListSxp(a,b))) => (rho1, a)
            | ("cdr", (rho1, ListSxp(a,b))) => (rho1, b)
            | ("number?", (rho1, NumSxp x)) => (rho1, SymSxp "T")
            | ("symbol?", (rho1, SymSxp x)) => (rho1, SymSxp "T")
            | ("list?", (rho1, ListSxp x)) => (rho1, SymSxp "T")
            | ("null?", (rho1, NilSxp)) => (rho1, SymSxp "T")
            | (_ , (rho1, _ )) => (rho1, NilSxp)
            | _ => raise RuntimeError ("Unknown unary operator: "^name)
        )

    | eval (rho, ApExp (name, args)) =
        (case functionGet name of
            NONE => raise RuntimeError ("Undefined function: "^name)
            | SOME (name, formals, body) =>
                let fun f rho1 [] [] newRho =
                    (case eval (newRho, body) of (_, value) => (rho1, value))
                | f rho1 (x::xs) (y::ys) newRho =
                    (case eval (rho1, y) of (rho2, value) =>
                        f rho2 xs ys (rhoSet newRho x value))
                | f _ _ _ _ = raise RuntimeError ("Wrong number of arguments to: "^name)
                in f rho formals args [] end)