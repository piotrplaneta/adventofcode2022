open Base

let test_input =
  [
    "Sensor at x=2, y=18: closest beacon is at x=-2, y=15";
    "Sensor at x=9, y=16: closest beacon is at x=10, y=16";
    "Sensor at x=13, y=2: closest beacon is at x=15, y=3";
    "Sensor at x=12, y=14: closest beacon is at x=10, y=16";
    "Sensor at x=10, y=20: closest beacon is at x=10, y=16";
    "Sensor at x=14, y=17: closest beacon is at x=10, y=16";
    "Sensor at x=8, y=7: closest beacon is at x=2, y=10";
    "Sensor at x=2, y=0: closest beacon is at x=2, y=10";
    "Sensor at x=0, y=11: closest beacon is at x=2, y=10";
    "Sensor at x=20, y=14: closest beacon is at x=25, y=17";
    "Sensor at x=17, y=20: closest beacon is at x=21, y=22";
    "Sensor at x=16, y=7: closest beacon is at x=15, y=3";
    "Sensor at x=14, y=3: closest beacon is at x=15, y=3";
    "Sensor at x=20, y=1: closest beacon is at x=15, y=3";
  ]

let input = Files.read_lines "lib/day15.input"

module IntPair = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

type grid_object = Sensor | Beacon | Nothing

let int_pair_map = Map.empty (module IntPair)

let rec digits_and_commas s =
  match s with
  | [] -> []
  | ('0' .. '9' as d) :: rest -> d :: digits_and_commas rest
  | ',' :: rest -> ',' :: digits_and_commas rest
  | _ :: rest -> digits_and_commas rest

let parsed_input =
  test_input
  |> List.map ~f:(fun line ->
         digits_and_commas (String.to_list line)
         |> String.of_char_list |> String.split ~on:','
         |> fun coords ->
         match coords with
         | [ sx; sy; bx; by ] ->
             ( Int.of_string sx,
               Int.of_string sy,
               Int.of_string bx,
               Int.of_string by )
         | _ -> failwith "Error in data parsing")

let grid_distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let rec up_range from until =
  if from < until then from :: up_range (from + 1) until else []

let rec down_range from until =
  if from > until then from :: down_range (from - 1) until else []

let grid_range from d =
  List.concat [ down_range from (from - d); up_range from (from + d) ]
  |> List.remove_consecutive_duplicates ~equal:( = )

let update grid (x, y) v =
  match Map.find grid (x, y) with
  | None -> Map.add_exn ~key:(x, y) ~data:v grid
  | _ -> grid

let add_empty_holes_from_beacon (x, y) d grid =
  let x_range, y_range = (grid_range x d, grid_range y d) in
  let empty_slots =
    List.cartesian_product x_range y_range
    |> List.filter ~f:(fun (slot_x, slot_y) ->
           grid_distance (x, y) (slot_x, slot_y) < d)
  in
  List.fold ~init:grid
    ~f:(fun grid_acc (slot_x, slot_y) ->
      update grid_acc (slot_x, slot_y) Nothing)
    empty_slots

let init_grid =
  List.fold
    ~init:(Map.empty (module IntPair))
    ~f:(fun grid (sx, sy, bx, by) ->
      let grid_with_sensor = update grid (sx, sy) Sensor in
      let grid_with_beacon = update grid_with_sensor (bx, by) Beacon in
      add_empty_holes_from_beacon (sx, sy)
        (grid_distance (sx, sy) (bx, by))
        grid_with_beacon)
    parsed_input

let part_one =
  init_grid |> Map.filter_keys ~f:(fun (_, y) -> y = 10) |> Map.length
