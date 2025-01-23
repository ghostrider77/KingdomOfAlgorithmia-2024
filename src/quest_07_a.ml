module StringMap = Map.Make(String)


let parse_input (lines : string list) : string list StringMap.t =
  let parse line = Scanf.sscanf line "%s@:%s" (fun d actions -> (d, String.split_on_char ',' actions)) in
  lines |> List.map parse |> StringMap.of_list


let calc_total_power (action_list : string list) (nr_steps : int) : int =
  let actions = action_list |> List.to_seq |> Seq.cycle |> Seq.take nr_steps in
  let process_action (acc, power) = function
    | "+" -> (acc + power + 1, power + 1)
    | "-" -> (acc + power - 1, power - 1)
    | "=" -> (acc + power, power)
    | _ -> failwith "Unknown action" in
  fst @@ Seq.fold_left process_action (0, 10) actions


let rank_action_plans (plans : string list StringMap.t) (nr_steps : int) : string =
  plans
    |> StringMap.map (fun plan -> calc_total_power plan nr_steps)
    |> StringMap.bindings
    |> List.sort (fun (_, v1) (_, v2) -> compare v2 v1)
    |> List.map fst
    |> String.concat ""


let () =
  let lines = In_channel.input_lines stdin in
  let plans = parse_input lines in
  let nr_steps = 10 in
  let result = rank_action_plans plans nr_steps in
  print_endline result;
