let calc_nr_of_strikes (nails : int list) : int =
  let calc_median xs =
    let n = List.length xs in
    let sorted = List.sort compare xs in
    if n mod 2 = 1 then List.nth sorted (n / 2)
    else
      let m1 = List.nth sorted (n / 2 - 1) in
      let m2 = List.nth sorted (n / 2) in
      (m1 + m2) / 2 in
  let m = calc_median nails in
  List.fold_left (fun acc nail -> acc + abs (nail - m)) 0 nails


let () =
  let lines = In_channel.input_lines In_channel.stdin in
  let nails = List.map int_of_string lines in
  let result = calc_nr_of_strikes nails in
  print_int result; print_newline ()
