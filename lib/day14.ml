open Base

let test_input =
  [ "498,4 -> 498,6 -> 496,6"; "503,4 -> 502,4 -> 502,9 -> 494,9" ]

let input = Files.read_lines "lib/day14.input"

module IntPair = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

let rec generate_horizontal_line x1 x2 y =
  if x1 > x2 then generate_horizontal_line x2 x1 y
  else
    match x2 - x1 with
    | 0 -> [ (x1, y) ]
    | _ -> (x1, y) :: generate_horizontal_line (x1 + 1) x2 y

let rec generate_vertical_line y1 y2 x =
  if y1 > y2 then generate_vertical_line y2 y1 x
  else
    match y2 - y1 with
    | 0 -> [ (x, y1) ]
    | _ -> (x, y1) :: generate_vertical_line (y1 + 1) y2 x

let rec draw_lines points =
  match points with
  | (x1, y1) :: (x2, y2) :: rest when y1 = y2 ->
      generate_horizontal_line x1 x2 y1 :: draw_lines ((x2, y2) :: rest)
  | (x1, y1) :: (x2, y2) :: rest when x1 = x2 ->
      generate_vertical_line y1 y2 x1 :: draw_lines ((x2, y2) :: rest)
  | _ -> []

let parsed_input =
  input
  |> List.map ~f:(String.substr_replace_all ~pattern:" -> " ~with_:"$")
  |> List.map ~f:(fun line ->
         let points = String.split ~on:'$' line in
         List.map
           ~f:(fun point ->
             String.split ~on:',' point |> List.map ~f:Int.of_string
             |> fun point_list ->
             (List.hd_exn point_list, List.hd_exn (List.tl_exn point_list)))
           points)

let lines = List.map ~f:draw_lines parsed_input
let flattened_lines = List.concat (List.concat lines)
let rock_grid = Set.of_list (module IntPair) flattened_lines

let max_rock_y =
  rock_grid |> Set.to_list
  |> List.map ~f:(fun (_, y) -> y)
  |> List.fold ~f:max ~init:0

let rec drop_sand_part_one (from_x, from_y) grid =
  match Set.mem grid (from_x, from_y + 1) with
  | _ when from_y + 1 > max_rock_y -> Set.length grid
  | false -> drop_sand_part_one (from_x, from_y + 1) grid
  | true when not (Set.mem grid (from_x - 1, from_y + 1)) ->
      drop_sand_part_one (from_x - 1, from_y + 1) grid
  | true when not (Set.mem grid (from_x + 1, from_y + 1)) ->
      drop_sand_part_one (from_x + 1, from_y + 1) grid
  | true -> drop_sand_part_one (500, 0) (Set.add grid (from_x, from_y))

let part_one = drop_sand_part_one (500, 0) rock_grid - Set.length rock_grid

let rec drop_sand_part_two (from_x, from_y) grid =
  match Set.mem grid (from_x, from_y) with
  | true when from_x = 500 && from_y = 0 -> Set.length grid
  | true -> failwith "Incorrect state of falling"
  | false
    when (not (Set.mem grid (from_x, from_y + 1))) && from_y < max_rock_y + 1 ->
      drop_sand_part_two (from_x, from_y + 1) grid
  | false
    when (not (Set.mem grid (from_x - 1, from_y + 1)))
         && from_y < max_rock_y + 1 ->
      drop_sand_part_two (from_x - 1, from_y + 1) grid
  | false
    when (not (Set.mem grid (from_x + 1, from_y + 1)))
         && from_y < max_rock_y + 1 ->
      drop_sand_part_two (from_x + 1, from_y + 1) grid
  | false -> drop_sand_part_two (500, 0) (Set.add grid (from_x, from_y))

let part_two = drop_sand_part_two (500, 0) rock_grid - Set.length rock_grid
