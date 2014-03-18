let fib self = function
  | 0 -> 0
  | 1 -> 1
  | n -> self (n - 1) + self (n - 2)

(*
 * F(n + 2) = F(n + 1) + F(n)
 * F(n + 3) = F(n + 2) + F(n + 1) = F(n + 1) + F(n + 1) + F(n) = 2F(n + 1) + F(n)
 * F(n + 4) = F(n + 3) + F(n + 2) = F(n + 1) + F(n) + 2F(n + 1) + F(n)
 *          = 3F(n + 1) + 2F(n)
 *)

let odd self = function
  | 0 -> (1, 1) (* F(0), F(1) *)
  | n ->
    let (a, b) = self (n - 1)
    in (a + 2 * b, 2 * a + 3 * b)
    (* F(n + 3), F(n + 4) *)

(*
 * P(0) = F(0) = 0
 * P(1) = F(3) = 2
 * P(2) = F(6) = 8
 * P(n) = F(3n)
 *
 * P(n) = F(3n) = a
 * F(3n + 1) = b
 * F(3n + 2) = a + b
 * P(n + 1) = F(3n + 3) = a + b + b = a + 2b
 * F(3n + 4) = a + b + a + 2b = 2a + 3b
 * F(3n + 5) = a + 2b + 2a + 3b = 3a + 5b
 * P(n + 2) = F(3n + 6) = 2a + 3b + 3a + 5b = 5a + 8b
 *          = a + 4a + 8b = a + 4(a + 2b)
 *          = F(3n) + 4F(3n + 3)
 *          = P(n) + 4P(n + 1)
 *)

let ppp self = function
  | 0 -> 0
  | 1 -> 2
  | n -> self (n - 2) + 4 * (self (n - 1)) (* P(n) * 4P(n + 1) *)

let memoize f =
  let cache = Hashtbl.create 10 in
  let rec aux x =
    try Hashtbl.find cache x
    with _ ->
      let y = f aux x in
      Hashtbl.add cache x y; y
  in aux

let modd = memoize odd
let mppp = memoize ppp

(*
 * S = E(k = 0 -> n)(P(k)) with P(k) < n
 * P(k) = P(k - 2) + 4P(k - 1) with k >= 2
 *
 * E(k = 2 -> n + 1)(P(k)) = E(k = 2 -> n + 1)(P(k - 2)) + 4E(k = 2 -> n + 1)(P(k - 1))
 *
 * E(k = 2 -> n + 1)(P(k)) + P0 + P1 - P(n + 1) = S(n)
 * E(k = 2 -> n + 1)(P(k)) + 2 - P(n + 1) = S(n)
 * E(k = 2 -> n + 1)(P(k)) = S(n) - 2 + P(n + 1)
 *
 * E(k = 2 -> n + 1)(P(k - 2)) = E(k = 0 -> n - 1)(P(k))
 * E(k = 0 -> n - 1)(P(k)) + P(n) = S(n)
 * E(k = 0 -> n - 1)(P(k)) = S(n) - P(n)
 *
 * 4E(k = 2 -> n + 1)(P(k - 1)) = 4E(k = 0 -> n)
 *                              = 4S(n)
 *
 * S(n) - 2 + P(n + 1) = S(n) - P(n) + 4S(n)
 * S(n) - 2 + P(n + 1) = 5S(n) - P(n)
 * P(n) - 2 + P(n + 1) = 4S(n)
 * S(n) = (P(n) - 2 + P(n + 1)) / 4
 *)

let sum n = (mppp n - 2 + mppp (n + 1)) / 4
let max =
  let rec aux x = if mppp x >= 4000000 then x - 1 else aux (x + 1)
  in aux 0

let () = print_int (sum max)
