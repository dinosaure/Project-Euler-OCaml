let rec div n d = match n mod d with
  | 0 -> d
  | _ -> div n (d + 1)

let rec fprime ?(d=2) n =
  let d = div n d in
  if n = d then n
  else fprime ~d (n / d)

let () = print_int (fprime 600851475143)
