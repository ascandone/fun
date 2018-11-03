use "ast.sml";
use "lazyStatic.sml";

datatype myvars = x | y | z

fun eval p = EmptyEnv |- p

val p = x := SConst 7 @@ Var x
val p1 = x ~> y ~> SConst 7
val id = x ~> Var x
val p2 = apply id (SConst 42)
val es = x := SConst 7 @@ Apply (y ~> (x := SConst 4 @@ (Apply (Var y, SConst 9))), z ~> Var x)
val es1 = apply (x ~> apply (Var x)(Var x)) (x ~> apply (Var x) (Var x))
val p3 = apply (x ~> SConst 7) (Var x)

(* let x = x in x *)
val selfDef = x := Var x @@ Var x

(*

  let val f = (fn x => f x) in f 0 end; (ML:  Error: unbound variable or constructor: f)

  let f = (fun x -> f x) in f 0;;  (Ocaml: Error: Unbound value f)

  let f = \x -> f x in f 0 (HS: non termina )
  let f = \x -> if x == 0 then 0 else f (x - 1) in f 100 (HS: -> 0)

  (let [f, (fn [x] (f x))]
    (f 0)) (clj: CompilerException java.lang.RuntimeException: Unable to resolve symbol: f in this context, compiling:(NO_SOURCE_PATH:4:18) )

*)
val recur = "f" := ("x" ~> apply (Var "f") (Var "x")) @@ apply (Var "f") (SConst 0)


(* Church encodings *)
val cZero = "_" ~> "y" ~> Var "y"
val cSucc = "n" ~> "x" ~> "y" ~> apply (Var "x") (apply2 (Var "n") (Var "x") (Var "y"))

val cOne = apply cSucc cZero

val cTrue = "x" ~> "_" ~> Var "x"
val cFalse = "_" ~> "y" ~> Var "y"

val pair = "x" ~> "y" ~> "b" ~> apply2 (Var "b") (Var "x") (Var "y")

val fst = "pair" ~> apply (Var "pair") cTrue
val snd = "pair" ~> apply (Var "pair") cFalse

