let parse_input = function
  | [runic_words; ""; sentence] ->
      let words = Scanf.sscanf runic_words "WORDS:%s" (String.split_on_char ',')
      in words, sentence
  | _ -> failwith "Malformed input."


let count_occurrence (text : string) (word : string) : int =
  let n = String.length text in
  let k = String.length word in
  let rec loop acc ix =
    if ix >= n - k + 1 then acc
    else if (String.sub text ix k) = word then loop (acc + 1) (ix + 1)
    else loop acc (ix + 1)
  in loop 0 0


let count_all_words (text : string) (words : string list) : int =
  List.fold_left (fun acc word -> acc + count_occurrence text word) 0 words


let () =
  let lines = In_channel.input_lines In_channel.stdin in
  let words, sentence = parse_input lines in
  let result = count_all_words sentence words in
  print_int result; print_newline ()
