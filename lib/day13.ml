let explode s = String.to_seq s |> List.of_seq
let implode l = String.of_seq (List.to_seq l)

let rec remove_last l =
  match l with [] -> [] | [ _ ] -> [] | hd :: tl -> hd :: remove_last tl

let list_with_indices lst =
  let rec list_with_indices' lst i =
    match lst with
    | hd :: tl -> (i, hd) :: list_with_indices' tl (i + 1)
    | _ -> []
  in

  list_with_indices' lst 1

type expr = Number of int | Paren of expr list | Null

let rec index x lst =
  match lst with
  | [] -> failwith "Not found"
  | h :: t -> if x = h then 1 else 1 + index x t

let split_exprs line =
  let rec split_exprs' line depth acc =
    match line with
    | ',' :: rest when depth == 0 -> [ implode acc ] @ split_exprs' rest 0 []
    | '[' :: rest -> split_exprs' rest (depth + 1) (acc @ [ '[' ])
    | ']' :: rest -> split_exprs' rest (depth - 1) (acc @ [ ']' ])
    | x :: rest -> split_exprs' rest depth (acc @ [ x ])
    | _ -> [ implode acc ]
  in

  split_exprs' line 0 []

let rec take_while_digit l =
  match l with
  | ('0' .. '9' as d) :: rest ->
      let remaining_digits, not_digits = take_while_digit rest in
      (d :: remaining_digits, not_digits)
  | not_digits -> ([], not_digits)

let rec parse_expr expr_str =
  match expr_str with
  | '[' :: rest ->
      let inside_expr_str = remove_last rest in
      if inside_expr_str == [] then Paren []
      else
        Paren
          (List.map
             (fun part -> parse_expr part)
             (List.map explode (split_exprs inside_expr_str)))
  | i :: rest ->
      let digits, _ = take_while_digit (i :: rest) in
      Number (int_of_string (implode digits))
  | err -> failwith (implode err)

let rec zip_with_nulls l1 l2 =
  match (l1, l2) with
  | hd1 :: tl1, hd2 :: tl2 -> (hd1, hd2) :: zip_with_nulls tl1 tl2
  | [], hd2 :: tl2 -> (Null, hd2) :: zip_with_nulls [] tl2
  | hd1 :: tl1, [] -> (hd1, Null) :: zip_with_nulls tl1 []
  | [], [] -> []

let rec is_right_order expr_pair =
  match expr_pair with
  | Number n1, Number n2 ->
      if n1 < n2 then Some true else if n1 > n2 then Some false else None
  | Paren l1, Paren l2 ->
      List.fold_right
        (fun maybe acc ->
          match maybe with
          | Some false -> Some false
          | Some true -> Some true
          | None -> acc)
        (List.map is_right_order (zip_with_nulls l1 l2))
        None
  | Paren l1, Number n -> is_right_order (Paren l1, Paren [ Number n ])
  | Number n, Paren l2 -> is_right_order (Paren [ Number n ], Paren l2)
  | Number _, Null -> Some false
  | Paren _, Null -> Some false
  | Null, Number _ -> Some true
  | Null, Paren _ -> Some true
  | Null, Null -> failwith "Both nulls"

let rec expr_pairs lines =
  match lines with
  | l1 :: l2 :: "" :: rest ->
      (parse_expr (explode l1), parse_expr (explode l2)) :: expr_pairs rest
  | _ -> []

let part_one_input = Files.read_lines "lib/day13.input"

let part_one =
  expr_pairs part_one_input |> List.map is_right_order |> list_with_indices
  |> List.filter (fun (_, maybe) ->
         match maybe with Some true -> true | _ -> false)
  |> List.map (fun (i, _) -> i)
  |> List.fold_left ( + ) 0

let part_two_input = Files.read_lines "lib/day13_part2.input"

let sorted =
  List.sort
    (fun e1 e2 ->
      match is_right_order (e1, e2) with
      | Some true -> -1
      | Some false -> 1
      | None -> 0)
    (List.map (fun line -> parse_expr (explode line)) part_two_input)

let part_two =
  index (Paren [ Paren [ Number 2 ] ]) sorted
  * index (Paren [ Paren [ Number 6 ] ]) sorted
