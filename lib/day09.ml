let input = Files.read_lines "lib/day09.input"
let explode s = String.to_seq s |> List.of_seq

module IntPairs = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with 0 -> Stdlib.compare y0 y1 | c -> c
end

module PointsSet = Set.Make (IntPairs)

let initial_directions =
  input
  |> List.map (fun line ->
         match String.split_on_char ' ' line with
         | dir :: steps :: _ -> (
             match dir with
             | "R" -> ((1, 0), int_of_string steps)
             | "L" -> ((-1, 0), int_of_string steps)
             | "U" -> ((0, 1), int_of_string steps)
             | "D" -> ((0, -1), int_of_string steps)
             | _ -> failwith "bad input")
         | _ -> failwith "bad input")

let new_tail_coordinates (new_head_x, new_head_y) (tail_x, tail_y) =
  match (new_head_x - tail_x, new_head_y - tail_y) with
  | 2, 0 -> (tail_x + 1, tail_y)
  | 2, 1 -> (tail_x + 1, tail_y + 1)
  | 2, 2 -> (tail_x + 1, tail_y + 1)
  | 2, -1 -> (tail_x + 1, tail_y - 1)
  | 2, -2 -> (tail_x + 1, tail_y - 1)
  | -2, 0 -> (tail_x - 1, tail_y)
  | -2, 1 -> (tail_x - 1, tail_y + 1)
  | -2, 2 -> (tail_x - 1, tail_y + 1)
  | -2, -1 -> (tail_x - 1, tail_y - 1)
  | -2, -2 -> (tail_x - 1, tail_y - 1)
  | -1, 2 -> (tail_x - 1, tail_y + 1)
  | 0, 2 -> (tail_x, tail_y + 1)
  | 1, 2 -> (tail_x + 1, tail_y + 1)
  | -1, -2 -> (tail_x - 1, tail_y - 1)
  | 0, -2 -> (tail_x, tail_y - 1)
  | 1, -2 -> (tail_x + 1, tail_y - 1)
  | _ -> (tail_x, tail_y)

let rec visitedByTail (head_x, head_y) (tail_x, tail_y) directions
    (visited : PointsSet.t) =
  match directions with
  | ((dx, dy), steps) :: rest ->
      let new_head_x, new_head_y = (head_x + dx, head_y + dy) in
      let new_tail_x, new_tail_y =
        new_tail_coordinates (new_head_x, new_head_y) (tail_x, tail_y)
      in
      if steps == 1 then
        visitedByTail (new_head_x, new_head_y)
          (new_tail_coordinates (new_head_x, new_head_y) (tail_x, tail_y))
          rest
          (visited |> PointsSet.add (new_tail_x, new_tail_y))
      else
        visitedByTail (new_head_x, new_head_y) (new_tail_x, new_tail_y)
          (((dx, dy), steps - 1) :: rest)
          (visited |> PointsSet.add (new_tail_x, new_tail_y))
  | [] -> visited

let part_one =
  visitedByTail (0, 0) (0, 0) initial_directions PointsSet.empty
  |> PointsSet.elements |> List.length

let new_knots_positions (head_new_x, head_new_y) knots_positions =
  let rec new_positions' (prev_point_new_x, prev_point_new_y) knots_positions
      acc =
    match knots_positions with
    | [] -> acc
    | (point_x, point_y) :: rest ->
        let point_new_x, point_new_y =
          new_tail_coordinates
            (prev_point_new_x, prev_point_new_y)
            (point_x, point_y)
        in
        new_positions' (point_new_x, point_new_y) rest
          (acc @ [ (point_new_x, point_new_y) ])
  in

  new_positions' (head_new_x, head_new_y) knots_positions []

let rec last xs =
  match xs with
  | [ x ] -> x
  | _ :: rest -> last rest
  | [] -> failwith "empty list for last"

let rec visitedByTailLong (head_x, head_y) positions directions
    (visited : PointsSet.t) =
  match directions with
  | ((dx, dy), steps) :: rest ->
      let new_head_x, new_head_y = (head_x + dx, head_y + dy) in
      let new_knots_positions =
        new_knots_positions (new_head_x, new_head_y) positions
      in
      if steps == 1 then
        visitedByTailLong (new_head_x, new_head_y) new_knots_positions rest
          (visited |> PointsSet.add (last new_knots_positions))
      else
        visitedByTailLong (new_head_x, new_head_y) new_knots_positions
          (((dx, dy), steps - 1) :: rest)
          (visited |> PointsSet.add (last new_knots_positions))
  | [] -> visited

let init_knots_positions =
  [ (0, 0); (0, 0); (0, 0); (0, 0); (0, 0); (0, 0); (0, 0); (0, 0); (0, 0) ]

let part_two =
  visitedByTailLong (0, 0) init_knots_positions initial_directions
    PointsSet.empty
  |> PointsSet.elements |> List.length
