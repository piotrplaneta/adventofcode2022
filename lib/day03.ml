let input = Files.read_lines "lib/day03.input"
let explode_string s = List.init (String.length s) (String.get s)

let rec split_at l acc n =
  if n = 0 then (acc, l)
  else
    match l with
    | [] -> (acc, l)
    | head :: tail -> split_at tail (acc @ [ head ]) (n - 1)

let split_at_mid l = split_at l [] (List.length l / 2)
let same_chars (l1, l2) = List.filter (fun x -> List.exists (( == ) x) l2) l1
let same_chars_trio (l1, l2, l3) = same_chars (same_chars (l1, l2), l3)

let char_priority c =
  Char.code c - Char.code 'a' + 1 |> fun x -> if x < 0 then x + 58 else x

let part_one =
  List.map
    (fun l ->
      explode_string l |> split_at_mid |> same_chars |> List.hd |> char_priority)
    input
  |> List.fold_left ( + ) 0

let rec each_trios l =
  match l with
  | [] -> []
  | x :: y :: z :: rest ->
      (explode_string x, explode_string y, explode_string z) :: each_trios rest
  | _ -> []

let part_two =
  List.map
    (fun trio -> same_chars_trio trio |> List.hd |> char_priority)
    (each_trios input)
  |> List.fold_left ( + ) 0
