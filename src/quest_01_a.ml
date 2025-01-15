type creature = Ant | Beetle | Cockroach


let creature_of_char = function
  | 'A' -> Ant
  | 'B' -> Beetle
  | 'C' -> Cockroach
  | _ -> failwith "Unknown creature."


let calc_potion = function
  | Ant -> 0
  | Beetle -> 1
  | Cockroach -> 3


let () =
  let line = Option.get @@ In_channel.input_line In_channel.stdin in
  let creatures = line |> String.to_seq |> Seq.map creature_of_char in
  let result = Seq.fold_left (fun acc creature -> acc + calc_potion creature) 0 creatures in
  print_int result; print_newline ()
