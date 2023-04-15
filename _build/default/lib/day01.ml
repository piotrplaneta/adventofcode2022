let input = Files.read_lines "lib/day01.input"

let separate_lines_by_empty_line =
  List.fold_left
    (fun acc line ->
      match line with
      | "" -> [ [] ] @ acc
      | line -> [ [ int_of_string line ] @ List.hd acc ] @ List.tl acc)
    [ [] ] input

let sums_of_calories =
  List.map (List.fold_left ( + ) 0) separate_lines_by_empty_line

let part_a = sums_of_calories |> List.fold_left max min_int
let desc_sorted_sums = sums_of_calories |> List.sort compare |> List.rev

let part_b =
  List.hd desc_sorted_sums
  + List.hd (List.tl desc_sorted_sums)
  + List.hd (List.tl (List.tl desc_sorted_sums))
