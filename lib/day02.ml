let input = Files.read_lines "lib/day02.input"

let rounds =
  input
  |> List.map (fun line ->
         ( List.hd (String.split_on_char ' ' line),
           List.hd (List.tl (String.split_on_char ' ' line)) ))

let score_round = function
  | "A", "X" -> 4
  | "A", "Y" -> 8
  | "A", "Z" -> 3
  | "B", "X" -> 1
  | "B", "Y" -> 5
  | "B", "Z" -> 9
  | "C", "X" -> 7
  | "C", "Y" -> 2
  | "C", "Z" -> 6
  | _ -> 0

let part_one = rounds |> List.map score_round |> List.fold_left ( + ) 0

let score_round_two = function
  | "A", "X" -> 3
  | "A", "Y" -> 4
  | "A", "Z" -> 8
  | "B", "X" -> 1
  | "B", "Y" -> 5
  | "B", "Z" -> 9
  | "C", "X" -> 2
  | "C", "Y" -> 6
  | "C", "Z" -> 7
  | _ -> 0

let part_two = rounds |> List.map score_round_two |> List.fold_left ( + ) 0
