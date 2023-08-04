open Base

let input = [ "498,4 -> 498,6 -> 496,6"; "503,4 -> 502,4 -> 502,9 -> 494,9" ]

module IntPair = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

let int_pair_set = Set.empty (module IntPair)

let rec generate_horizontal_line x1 x2 y =
  if x1 > x2 then generate_horizontal_line x2 x1 y
  else
    match x2 - x1 with
    | 0 -> [ (x1, y) ]
    | _ -> (x1, y) :: generate_horizontal_line (x1 + 1) x2 y

let rec generate_vertical_line y1 y2 x =
  if y1 > y2 then generate_horizontal_line y2 y1 x
  else
    match y2 - y1 with
    | 0 -> [ (x, y1) ]
    | _ -> (x, y1) :: generate_horizontal_line (y1 + 1) y2 x

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
