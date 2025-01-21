let convert_to_intlist (line : string) : int list =
  List.map int_of_string Str.(line |> split (regexp "[ \t]+"))


let parse_input (lines : string list) : int list array =
  lines
    |> List.map (fun row -> List.to_seq (convert_to_intlist row))
    |> List.to_seq
    |> Seq.transpose
    |> Seq.map List.of_seq
    |> Array.of_seq


let shout_out (columns : int list array) : int =
  let heads = Array.map List.hd columns in
  heads |> Array.to_list |> List.map string_of_int |> String.concat "" |> int_of_string


let insert_clapper (column : int list) (clapper : int) : int list =
  let n = List.length column in
  let k =
    let ix = (clapper - 1) mod (2*n) in
    ix - max (2 * (ix - n)) 0 in
  let insert xs x ix =
    let first = List.take ix xs in
    let second = List.drop ix xs in
    List.append first (x :: second)
  in insert column clapper k


let play_rounds (dancers : int list array) : int =
  let nr_columns = Array.length dancers in
  let table = Hashtbl.create 1000 in
  let rec loop k =
    let j = k mod nr_columns in
    let column = dancers.(j) in
    let clapper = List.hd column in
    dancers.(j) <- List.tl column;
    let j' = (j + 1) mod nr_columns in
    let next_column = dancers.(j') in
    dancers.(j') <- insert_clapper next_column clapper;
    let state = Array.to_list dancers in
    let shouted = shout_out dancers in
    if Hashtbl.mem table state then Hashtbl.fold (fun _ v acc -> max acc v) table 0
    else
      (Hashtbl.add table state shouted;
      loop (k + 1))
  in loop 0


let () =
  let lines = In_channel.input_lines stdin in
  let columns = parse_input lines in
  let result = play_rounds columns in
  print_int result; print_newline ()
