let calc_nr_of_strikes (nails : int list) : int =
  let smallest = List.fold_left min max_int nails in
  List.fold_left (fun acc nail -> acc + (nail - smallest)) 0 nails


let () =
  let lines = In_channel.input_lines In_channel.stdin in
  let nails = List.map int_of_string lines in
  let result = calc_nr_of_strikes nails in
  print_int result; print_newline ()
