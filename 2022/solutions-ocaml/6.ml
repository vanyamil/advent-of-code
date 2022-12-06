(*
	Year: 2022
	Day: 6
	Name: 
*)

(* Header *)

#use "shared/utilities.ml" ;;

let input_file = file_to_line_seq "2022/inputs/6.txt" ;;

(* Main file *)

let incr_ c acc =
	match List.assoc_opt c acc with
	| None -> (c, 1) :: acc
	| Some v -> (c, v + 1) :: (List.remove_assoc c acc)

let decr_ c acc =
	let v' = List.assoc c acc - 1 in
	let acc' = List.remove_assoc c acc in
	if v' = 0
	then acc'
	else (c, v') :: acc'

let rec test s len acc_vals at =
	let unique = List.for_all (fun (_, cnt) -> cnt = 1) acc_vals in
	if unique then at
	else
	let acc' = acc_vals |> decr_ s.[at - len] |> incr_ s.[at] in
	test s len acc' (succ at)

let pre_test s len = 
	List.init len (fun x -> x)
	|> List.fold_left (fun acc i -> incr_ s.[i] acc) []

let run len =
	match Seq.uncons input_file with
	| None -> failwith "No input detected"
	| Some (first_line, _) ->
		let acc = pre_test first_line len in
		test first_line len acc len

let part1 = 
	run 4
;;

let part2 = 
	run 14
;;

(* Final print *)

print_results (string_of_int part1) (string_of_int part2) ;;
