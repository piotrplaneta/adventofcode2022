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

let rec digits_and_separators s =
  match s with
  | [] -> []
  | ('0' .. '9' as d) :: rest -> d :: digits_and_separators rest
  | '-' :: rest -> '-' :: digits_and_separators rest
  | (',' | ':') :: rest -> ',' :: digits_and_separators rest
  | _ :: rest -> digits_and_separators rest

let grid_distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let parsed_input =
  input
  |> List.map ~f:(fun line ->
         digits_and_separators (String.to_list line)
         |> String.of_char_list |> String.split ~on:','
         |> fun coords ->
         match coords with
         | [ s_sx; s_sy; s_bx; s_by ] ->
             let sx, sy, bx, by =
               ( Int.of_string s_sx,
                 Int.of_string s_sy,
                 Int.of_string s_bx,
                 Int.of_string s_by )
             in
             (sx, sy, bx, by, grid_distance (sx, sy) (bx, by))
         | _ -> failwith "Error in data parsing")

let min_x searched_y =
  parsed_input
  |> List.map ~f:(fun (sx, sy, _, _, d) -> sx - d + abs (sy - searched_y))
  |> List.sort ~compare:Int.compare
  |> List.hd_exn

let max_x searched_y =
  parsed_input
  |> List.map ~f:(fun (sx, sy, _, _, d) -> sx + d - abs (sy - searched_y))
  |> List.sort ~compare:(fun x y -> -Int.compare x y)
  |> List.hd_exn

let rec up_range from until =
  if from <= until then from :: up_range (from + 1) until else []

let cannot_be_beacon_count searched_y =
  up_range (min_x searched_y) (max_x searched_y)
  |> List.filter ~f:(fun x ->
         List.for_all
           ~f:(fun (_, _, bx, by, _) -> (not (bx = x)) || not (by = searched_y))
           parsed_input
         && List.exists
              ~f:(fun (sx, sy, _, _, d) ->
                grid_distance (x, searched_y) (sx, sy) <= d)
              parsed_input)
  |> List.length

let part_one = cannot_be_beacon_count 2000000

let is_beacon (x, y) =
  List.for_all
    ~f:(fun (sx, sy, _, _, d) ->
      x >= 0 && x <= 4000000 && y >= 0 && y <= 4000000
      && grid_distance (x, y) (sx, sy) > d)
    parsed_input

let process_border (sx, sy, _, _, d) =
  let rec process_border' (x, y) (dx, dy) =
    match (x, y) with
    | _ when is_beacon (x, y) -> Some (x, y)
    | _ when x = sx - d && y = sy + 1 -> None
    | _ when x = sx && y = sy - d - 1 -> process_border' (x + 1, y + 1) (1, 1)
    | _ when x = sx + d + 1 && y = sy -> process_border' (x - 1, y + 1) (-1, 1)
    | _ when x = sx && y = sy + d + 1 -> process_border' (x - 1, y - 1) (-1, -1)
    | _ -> process_border' (x + dx, y + dy) (dx, dy)
  in
  process_border' (sx - d - 1, sy) (1, -1)

let find_beacon =
  List.fold_until ~init:None
    ~f:(fun _ l ->
      match process_border l with
      | Some (x, y) -> Stop (x, y)
      | None -> Continue None)
    ~finish:(fun _ -> (-1, -1))
    parsed_input

let part_two = find_beacon |> fun (x, y) -> (x * 4000000) + y
