let s d n =
  (* Number of multiple of d in [1;N[ *)
  let m = (n - 1) / d
  (* sum of (1+2+...+m) * d *)
  in (m * (m + 1) * d / 2)

let () =
  print_int ((s 3 1000) + (s 5 1000) - (s 15 1000))
  (* { 3; 6; 9; ...; 30; 33; ... } + { 5; 10; 15; ...; 30; 35; ... } - { 15; 30; ... } *)
