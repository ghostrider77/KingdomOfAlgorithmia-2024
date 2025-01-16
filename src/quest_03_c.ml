type coord = { x : int; y : int }

module CoordSet = Set.Make(
  struct
    type t = coord
    let compare = compare
  end)

type grid = { mines : CoordSet.t; nr_rows : int; nr_cols : int }


let parse_input = function
  | (line :: _) as lines ->
    let nr_cols = String.length line in
    let nr_rows = List.length lines in
    let mines = lines
      |> List.to_seq
      |> Seq.mapi (fun x r -> Seq.filter_map (fun (y, c) -> if c = '#' then Some {x; y} else None) @@ String.to_seqi r)
      |> Seq.concat
      |> CoordSet.of_seq in
    {mines; nr_rows; nr_cols}
  | _ -> failwith "Empty input."


let calc_nr_earth_blocks ({mines; _} : grid) : int =
  let get_neighbors {x; y} = [
      {x = x - 1; y}; {x = x - 1; y = y + 1}; {x; y = y + 1}; {x = x + 1; y = y + 1};
      {x = x + 1; y}; {x = x + 1; y = y - 1}; {x; y = y - 1}; {x = x - 1; y = y - 1}
    ] in
  let rec loop acc blocks =
    if CoordSet.is_empty blocks then acc
    else
      let is_inner_coord coord =
        let neighbors = get_neighbors coord in
        List.for_all (fun n -> CoordSet.mem n blocks) neighbors in
      let inner_part = CoordSet.filter is_inner_coord blocks in
      loop (acc + CoordSet.cardinal blocks) inner_part
  in loop 0 mines


let () =
  let lines = In_channel.input_lines In_channel.stdin in
  let grid = parse_input lines in
  let result = calc_nr_earth_blocks grid in
  print_int result; print_newline ()
