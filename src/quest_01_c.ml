type creature = Ant | Beetle | Cockroach | Dragonfly


let creature_option_of_char = function
  | 'A' -> Some Ant
  | 'B' -> Some Beetle
  | 'C' -> Some Cockroach
  | 'D' -> Some Dragonfly
  | 'x' -> None
  | _ -> failwith "Unknown creature."


let collect_triples (chars : char list) : (creature option * creature option * creature option) list =
  let rec loop acc = function
    | c1 :: c2 :: c3 :: rest ->
        let pair = (creature_option_of_char c1, creature_option_of_char c2, creature_option_of_char c3) in
        loop (pair :: acc) rest
    | _ -> List.rev acc
  in loop [] chars


let calc_potion = function
  | Ant -> 0
  | Beetle -> 1
  | Cockroach -> 3
  | Dragonfly -> 5


let calc_potion_for_creature_triples = function
  | (Some c1, Some c2, Some c3) -> 6 + calc_potion c1 + calc_potion c2 + calc_potion c3
  | (Some c1, Some c2, None) -> 2 + calc_potion c1 + calc_potion c2
  | (Some c1, None, Some c2) -> 2 + calc_potion c1 + calc_potion c2
  | (None, Some c1, Some c2) -> 2 + calc_potion c1 + calc_potion c2
  | (Some c, None, None) -> calc_potion c
  | (None, Some c, None) -> calc_potion c
  | (None, None, Some c) -> calc_potion c
  | _ -> 0


let () =
  let line = Option.get @@ In_channel.input_line In_channel.stdin in
  let creature_triples = line |> String.to_seq |> List.of_seq |> collect_triples in
  let result = List.fold_left (fun acc pair -> acc + calc_potion_for_creature_triples pair) 0 creature_triples in
  print_int result; print_newline ()
