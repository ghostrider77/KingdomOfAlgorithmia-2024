type coord = { x : int; y : int }

module CoordSet = Set.Make(
  struct
    type t = coord
    let compare = Stdlib.compare
  end)

type armour = { scales : char array array; nr_rows : int; nr_cols : int }


let parse_input = function
  | first :: "" :: ((line :: _) as sentences) ->
    let words = Scanf.sscanf first "WORDS:%s" (String.split_on_char ',') in
    let reverse_word w =
      let k = String.length w in
      String.init k (fun ix -> w.[k - ix - 1]) in
    let ws = List.sort_uniq compare (List.map reverse_word words) @ words in
    let nr_cols = String.length line in
    let nr_rows = List.length sentences in
    let scales = Array.of_list @@ List.map (Fun.compose Array.of_seq String.to_seq) sentences in
    {scales; nr_rows; nr_cols}, ws
  | _ -> failwith "Malformed input."


let find_word_in_armour ({scales; nr_rows; nr_cols} : armour) (word : string) : CoordSet.t =
  let all_coords = Seq.flat_map (fun y -> Seq.init nr_rows (fun x -> {x; y})) @@ Seq.init nr_cols Fun.id in
  let k = String.length word in
  let check_word_horizontally {x; y} =
    let word_coords = Seq.init k (fun offset -> {x; y = (y + offset) mod nr_cols}) in
    let s = String.of_seq @@ Seq.map (fun {x = ix; y = jy} -> scales.(ix).(jy)) word_coords in
    if s = word then CoordSet.of_seq word_coords else CoordSet.empty in
  let check_word_vertically {x; y} =
    if x + k > nr_rows then CoordSet.empty
    else
      let word_coords = Seq.init k (fun offset -> {x = x + offset; y}) in
      let s = String.of_seq @@ Seq.map (fun {x = ix; y = jy} -> scales.(ix).(jy)) word_coords in
      if s = word then CoordSet.of_seq word_coords else CoordSet.empty in
  let horizontal =
    Seq.fold_left (fun acc c -> CoordSet.union acc (check_word_horizontally c)) CoordSet.empty all_coords in
  Seq.fold_left (fun acc c -> CoordSet.union acc (check_word_vertically c)) horizontal all_coords


let count_scales_with_hidden_words (armour : armour) (words : string list) : int =
  words
    |> List.fold_left (fun acc word -> CoordSet.union acc (find_word_in_armour armour word)) CoordSet.empty
    |> CoordSet.cardinal


let () =
  let lines = In_channel.input_lines In_channel.stdin in
  let armour, words = parse_input lines in
  let result = count_scales_with_hidden_words armour words in
  print_int result; print_newline ()
