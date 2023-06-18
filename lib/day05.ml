let input = Files.read_lines "lib/day05.input"
let explode s = String.to_seq s |> List.of_seq

exception Invalid_input

let rec split_on_empty_line lines acc =
  match lines with
  | "" :: rest -> (acc, rest)
  | hd :: rest -> split_on_empty_line rest (acc @ [ hd ])
  | _ -> (acc, [])

let stack_lines, move_lines = split_on_empty_line input []

let stacks =
  let init_stacks = Array.init 9 (fun _ -> []) in
  stack_lines |> List.map explode
  |> List.map (fun line ->
         let line_array = Array.of_list line in
         List.init 9 (fun x -> x)
         |> List.map (fun index ->
                match Array.get line_array ((index * 4) + 1) with
                | 'A' .. 'Z' as crate ->
                    Array.set init_stacks index
                      (Array.get init_stacks index @ [ crate ])
                | _ -> ()))
  |> fun _ -> init_stacks

let parsed_moves =
  move_lines
  |> List.map (fun line ->
         explode line
         |> List.filter (fun c ->
                match c with '0' .. '9' -> true | ' ' -> true | _ -> false)
         |> List.to_seq |> String.of_seq |> String.split_on_char ' '
         |> fun ns ->
         match ns with
         | "" :: count :: "" :: from :: "" :: dest :: _ ->
             ( int_of_string count,
               int_of_string from - 1,
               int_of_string dest - 1 )
         | _ -> raise Invalid_input)

let rec take xs n =
  match xs with
  | [] -> failwith "Take n too big"
  | head :: rest -> if n = 1 then [ head ] else head :: take rest (n - 1)

let rec drop xs n =
  match xs with
  | [] -> failwith "Drop n too big"
  | _ :: rest -> if n = 1 then rest else drop rest (n - 1)

let mutated_stacks_part_one =
  let mutated_stacks = Array.copy stacks in
  parsed_moves
  |> List.map (fun (count, from, dest) ->
         let slice = take (Array.get mutated_stacks from) count in
         ( Array.set mutated_stacks dest
             (List.rev slice @ Array.get mutated_stacks dest),
           Array.set mutated_stacks from
             (drop (Array.get mutated_stacks from) count) ))
  |> fun _ -> mutated_stacks

let mutated_stacks_part_two =
  let mutated_stacks = Array.copy stacks in
  parsed_moves
  |> List.map (fun (count, from, dest) ->
         let slice = take (Array.get mutated_stacks from) count in
         ( Array.set mutated_stacks dest (slice @ Array.get mutated_stacks dest),
           Array.set mutated_stacks from
             (drop (Array.get mutated_stacks from) count) ))
  |> fun _ -> mutated_stacks

let part_one =
  mutated_stacks_part_one
  |> Array.map (fun s -> List.hd s)
  |> Array.to_list |> List.to_seq |> String.of_seq

let part_two =
  mutated_stacks_part_two
  |> Array.map (fun s -> List.hd s)
  |> Array.to_list |> List.to_seq |> String.of_seq
