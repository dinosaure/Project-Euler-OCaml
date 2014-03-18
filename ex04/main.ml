(*
 * p = abccba
 * p = 100001a + 10010b + 1100c
 * p = 11 * (9091a + 910b + 100c)
 * p = 11i * j
 *)

let rec compare l1 l2 = match l1, l2 with
  | [], [] -> true
  | (x1 :: r1), (x2 :: r2) when x1 = x2 -> compare r1 r2
  | _, _ -> false

let explode number =
  let rec aux acc number =
    if number < 10 then number :: acc
    else aux (number mod 10 :: acc) (number / 10)
  in aux [] number

let is_palindrom e = let l = explode e in compare l (List.rev l)

let rec make f acc a b next = match a with
  | i when i = b -> f acc b
  | n -> make f (f acc n) (next n) b next

let (a, b, p) =
  let auxj i (a, b, p) j =
    let e = i * j in
    if e > p && is_palindrom e then (i, j, e) else (a, b, p) in
  let auxi acc i =
    make (auxj i) acc i 1000 (fun x -> x + 1) in
  make auxi (0, 0, 0) 110 990 (fun x -> x + 11)

let () = print_int p
