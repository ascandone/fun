exception UnboundVariable
exception Parsing

infix 5 :+ (* syntactic "+" *)
infixr 5 ~>
infix 5 |-

datatype 'v ast = 
  SConst of int
  | Var of 'v
  | :+ of 'v ast * 'v ast 
  | Let of 'v * 'v ast * 'v ast
  | ~> of 'v * 'v ast
  | Apply of 'v ast * 'v ast

(* syntactic sugar *)

infix 5 :=
infix 5 @@

datatype 'v Let_ = := of 'v * 'v ast
  
fun (x := m) @@ n = Let (x, m, n)

fun apply f a = Apply (f, a)
fun apply2 f a b = Apply (apply f a, b)
fun apply3 f a b c = Apply (apply2 f a b, c)

infix 5 aug
infix 5 <-