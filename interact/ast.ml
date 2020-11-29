open Types

type ast = Ast of sym * ast list

let list_product (l1 : 'a list) (l2 : ('a list) list) : ('a list) list =
  List.concat (List.map (fun l -> List.map (List.cons l) l2) l1)

let rec ast_list_of_exp (e : exp) : ast list =
  match e.e' with
  | Tok _       -> []
  | Seq (s, es) -> List.map (fun es' -> Ast (s, es'))
                     (List.fold_right list_product (List.map ast_list_of_exp es) [[]])
  | Alt (es)    -> List.concat (List.map ast_list_of_exp !es)

let ast_list_of_exp_list (es : exp list) : ast list =
  List.concat (List.map ast_list_of_exp es)

let interpret_calc (ast : ast) : int =
  let rec get_int (ast_list : ast list) (factor : int) : int =
    match ast_list with
    | Ast (sym1, []) :: Ast (sym2, []) :: [] ->
      (int_of_string sym1 * factor * 10) + (int_of_string sym2 * factor)       
    | Ast (sym1, ast_list') :: Ast (sym2, []) :: [] ->
       get_int ast_list' (factor * 10) + (int_of_string sym2 * factor)
    | _ -> failwith "nums ast list should contain exactly 2 elements"
  in
  let rec traverse (ast : ast) : int =
    match ast with
    | Ast (sym, ast_list) when sym = "nums" -> get_int ast_list 1
    | Ast (sym, ast_list) when sym = "s0_plus_s1" ->
       (traverse @@ List.nth ast_list 0) + (traverse @@ List.nth ast_list 2)
    | Ast (sym, ast_list) when sym = "s0_minus_s1" ->
       (traverse @@ List.nth ast_list 0) - (traverse @@ List.nth ast_list 2)
    | Ast (sym, ast_list) when sym = "s1_times_s2" ->
       (traverse @@ List.nth ast_list 0) * (traverse @@ List.nth ast_list 2)
    | Ast (sym, ast_list) when sym = "s1_divide_s2" ->
       (traverse @@ List.nth ast_list 0) / (traverse @@ List.nth ast_list 2)
    | Ast (sym, ast_list) when sym = "brackets" ->
       traverse @@ List.nth ast_list 1
    | Ast (sym, _) -> int_of_string sym
  in traverse ast
