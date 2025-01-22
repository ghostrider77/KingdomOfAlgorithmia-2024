module IntMap = Map.Make(Int)
module StringMap = Map.Make(String)

type tree = Apple | Leaf of {name : string} | Node of {name : string; children : tree list}


let parse_input (lines : string list) : tree =
  let split line = Scanf.sscanf line "%s@:%s" (fun node neighbors -> (node, String.split_on_char ',' neighbors)) in
  let adjacency_list = StringMap.of_list (List.map split lines) in
  let rec create_tree = function
    | "@" -> Apple
    | name ->
        match StringMap.find_opt name adjacency_list with
          | None -> Leaf {name}
          | Some neighbors -> Node {name; children = List.map create_tree neighbors}
  in create_tree "RR"


let rec get_paths_to_apples = function
  | Apple -> [["@"]]
  | Leaf _ -> []
  | Node {name; children} -> List.(children |> concat_map get_paths_to_apples |> map (fun path -> name :: path))


let calc_singular_length_path (tree : tree) : string =
  let all_paths = get_paths_to_apples tree in
  let process_path acc path =
    let length = List.length path in
    let key = String.concat "" path in
    StringMap.add key length acc in
  let path_lengths = List.fold_left process_path StringMap.empty all_paths in
  let items = StringMap.bindings path_lengths in
  let update_counter acc len =
    let count = Option.value (IntMap.find_opt len acc) ~default:0 in
    IntMap.add len (count + 1) acc in
  let counter = List.fold_left (fun acc (_, len) -> update_counter acc len) IntMap.empty items in
  let unique_length = counter |> IntMap.to_seq |> Seq.find (fun (_, count) -> count = 1) |> Option.get |> fst in
  fst @@ List.find (fun (_, length) -> length = unique_length) items


let () =
  let lines = In_channel.input_lines stdin in
  let tree = parse_input lines in
  let result = calc_singular_length_path tree in
  print_endline result;
