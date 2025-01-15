module IntSet = Set.Make(Int)


let parse_input = function
  | runic_words :: "" :: sentences ->
      let words = Scanf.sscanf runic_words "WORDS:%s" (String.split_on_char ',')
      in words, sentences
  | _ -> failwith "Malformed input."


let get_covered_indices (text : string) (word : string) : IntSet.t =
  let n = String.length text in
  let k = String.length word in
  let rec loop acc ix =
    if ix >= n - k + 1 then acc
    else if (String.sub text ix k) <> word then loop acc (ix + 1)
    else loop (IntSet.add_seq (Seq.init k (fun j -> j + ix)) acc) (ix + 1)
  in loop IntSet.empty 0


let nr_of_covered_symbols (text : string) (words : string list) : int =
  words
    |> List.fold_left (fun acc word -> IntSet.union acc (get_covered_indices text word)) IntSet.empty
    |> IntSet.cardinal


let count_runic_symbols (texts : string list) (words : string list) : int =
  let reverse_word w =
    let k = String.length w in
    String.init k (fun ix -> w.[k - ix - 1]) in
  let ws = List.sort_uniq Stdlib.compare (List.map reverse_word words) @ words in
  List.fold_left (fun acc text -> acc + nr_of_covered_symbols text ws) 0 texts


let () =
  let lines = In_channel.input_lines In_channel.stdin in
  let words, sentences = parse_input lines in
  let result = count_runic_symbols sentences words in
  print_int result; print_newline ()
