let input = Files.read_lines "lib/day04.input"
let explode_string s = List.init (String.length s) (String.get s)
let ranges = List.map (String.split_on_char ',') input

exception Invalid_input

let parse_line line =
  match line with
  | first :: second :: _ ->
      (String.split_on_char '-' first, String.split_on_char '-' second)
  | _ -> raise Invalid_input

let parse_ranges parsed_line =
  match parsed_line with
  | l1 :: r1 :: _, l2 :: r2 :: _ ->
      ( (int_of_string l1, int_of_string r1),
        (int_of_string l2, int_of_string r2) )
  | _ -> raise Invalid_input

let ranges_list = List.map (fun line -> parse_line line |> parse_ranges) ranges

let are_inclusive ranges =
  match ranges with
  | (l1, r1), (l2, r2) when l1 <= l2 && r2 <= r1 -> true
  | (l1, r1), (l2, r2) when l2 <= l1 && r1 <= r2 -> true
  | _ -> false

let are_overlapping ranges =
  match ranges with
  | (l1, r1), (l2, r2) when r1 >= l2 && l1 <= r2 -> true
  | (l1, r1), (l2, r2) when r2 >= l1 && l2 <= r1 -> true
  | _ -> false

let part_one = List.filter are_inclusive ranges_list |> List.length
let part_two = List.filter are_overlapping ranges_list |> List.length
