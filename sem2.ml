open Common

(* type ('r, 's, 'a) m = ('a -> ('r -> 's) -> 's) -> ('r -> 's) -> 's *)

(*
type value =
  | Int of int
  | Fun of (value -> (out, out, value) mcont)
and out = value
*)

type ('a, 'r) mcont = 'a -> 'r
type ('a, 'r1, 'r2) cont = 'a -> ('r1, 'r2) mcont -> 'r2
type ('a, 'r1, 'r2) m = ('a, 'r1, 'r2) cont -> ('r1, 'r2) mcont -> 'r2

type value =
  | Int of int
  | Fun of (value -> (value, out, out) m)
and out = value

let ($) = function
  | Int _ -> failwith "type error: ($)"
  | Fun f -> f

let get_int = function
  | Int i -> i
  | Fun _ -> failwith "type error: get_int"

let value_to_string = function
  | Int i -> string_of_int i
  | Fun _ -> "<fun>"


let id x = x
let cps_id x g = g x

let succ = Fun(fun x k -> k (Int(get_int x + 1)))

let int_op op =
  Fun(fun x k -> k (Fun(fun y k' -> k' (Int(op (get_int x) (get_int y))))))


let rec sem expr env k = match expr with
  | Lit(i) -> k (Int(i))
  | Var(x) -> k (get x env)
  | Lam(x, e) -> k (Fun(fun v k' g' -> sem e (extend x v env) k' g'))
  | App(e1, e2) -> sem e1 env (fun f -> sem e2 env (fun a -> (f $ a) k))
  | Callcc(e) -> sem e env (fun f -> (f $ Fun(fun v _k' -> k v)) k)
  | Shift(e) ->
    sem e env (fun f -> (f $ Fun(fun v k' g'' -> k v (fun w -> k' w g''))) cps_id)
  | Reset(e) -> fun g -> sem e env cps_id (fun r -> k r g)

let init_env = empty
  |> extend "succ" succ
  |> extend "zero" (Int 0)
  |> extend "+" (int_op ( + ))
  |> extend "*" (int_op ( * ))

let run expr = sem expr init_env cps_id id
