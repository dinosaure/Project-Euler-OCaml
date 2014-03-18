let rec pgcd (a, b) = match b with
  | 0 -> a
  | n -> pgcd (b, a mod b)

let rec make f acc a b next = match a with
  | i when i = b -> f acc b
  | n -> make f (f acc n) (next n) b next

(*
 * pi(n) = ppcm(1 .. n)
 * pi(1) = 1
 * pi(n) = n \/ pi(n - 1) = (n * pi(n - 1)) / (n /\ pi(n - 1))
 *)

let pi n = make
  (fun acc i -> i * acc / pgcd (i, acc)) 1 2 (n + 1) (fun x -> x + 1)
  (* n * pi(n - 1) / (n /\ pi(n - 1)) *)

let () = print_int (pi 20)
