module StringMap = Map.Make(String)

let track_layout = [
  "S-=++=-==++=++=-=+=-=+=+=--=-=++=-==++=-+=-=+=-=+=+=++=-+==++=++=-=-=--";
  "-                                                                     -";
  "=                                                                     =";
  "+                                                                     +";
  "=                                                                     +";
  "+                                                                     =";
  "=                                                                     =";
  "-                                                                     -";
  "--==++++==+=+++-=+=-=+=-+-=+-=+-=+=-=+=--=+++=++=+++==++==--=+=++==+++-";
]


let parse_layout (rows : string list) : string =
  let reverse s =
    let n = String.length s in
    String.init n (fun ix -> s.[n - ix - 1]) in
  let nr_rows = List.length rows in
  let nr_cols = String.length (List.hd rows) in
  let top = String.sub (List.hd rows) 1 (nr_cols - 1) in
  let bottom = reverse @@ List.nth rows (nr_rows - 1) in
  let middle_rows = List.(rows |> tl |> rev |> tl |> rev) in
  let left = String.concat "" @@ List.rev @@ List.map (fun r -> String.make 1 r.[0]) middle_rows in
  let right = String.concat "" @@ List.map (fun r -> String.make 1 r.[nr_cols - 1]) middle_rows in
  top ^ right ^ bottom ^ left ^ "S"


let parse_input (lines : string list) : string list StringMap.t =
  let parse line = Scanf.sscanf line "%s@:%s" (fun d actions -> (d, String.split_on_char ',' actions)) in
  lines |> List.map parse |> StringMap.of_list


let calc_total_power (track : string) (action_list : string list) (nr_loops : int) : int =
  let track = track |> String.to_seq |> Seq.map (fun c -> String.make 1 c) |> Seq.cycle in
  let actions = action_list |> List.to_seq |> Seq.cycle in
  let rec loop (acc, power) xs k = match Seq.uncons xs with
    | Some ((_, "+"), rest) -> loop (acc + power + 1, power + 1) rest k
    | Some ((_, "-"), rest) -> loop (acc + power - 1, power - 1) rest k
    | Some ((action, t), rest) ->
        let acc', power' = match action with
          | "+" -> (acc + power + 1, power + 1)
          | "-" -> (acc + power - 1, power - 1)
          | "=" -> (acc + power, power)
          | _ -> failwith "Unknown action" in
        let k' = if t = "S" then k + 1 else k in
        if k' = nr_loops then acc' else loop (acc', power') rest k'
    | None -> failwith "Run out of actions" in
  loop (0, 10) (Seq.zip actions track) 0


let rank_action_plans (track : string) (plans : string list StringMap.t) (nr_loops : int) : string =
  plans
    |> StringMap.map (fun plan -> calc_total_power track plan nr_loops)
    |> StringMap.bindings
    |> List.sort (fun (_, v1) (_, v2) -> compare v2 v1)
    |> List.map fst
    |> String.concat ""


let () =
  let lines = In_channel.input_lines stdin in
  let plans = parse_input lines in
  let track = parse_layout track_layout in
  let nr_loops = 10 in
  let result = rank_action_plans track plans nr_loops in
  print_endline result;
