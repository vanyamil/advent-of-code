(*
	Year: 2022
	Day: 3
	Name: Rucksack Reorganization
*)

(* Header *)

#use "shared/utilities.ml" ;;

let input_file = file_to_line_seq "2022/inputs/3.txt" ;;

(* Main file *)

let priority c =
	(* Uppercase have lower ASCII value *)
	let a_l = Char.code 'a' in
	let a_u = Char.code 'A' in
	let i = Char.code c in
	if i <= Char.code 'Z'
	then i - a_u + 27
	else i - a_l + 1

let rec find_priority cur_idx s =
	let l = String.length s / 2 in
	if String.contains_from s l s.[cur_idx]
	then priority s.[cur_idx]
	else find_priority (succ cur_idx) s

let part1 = 
	input_file
	|> Seq.map (find_priority 0)
	|> Seq.fold_left (+) 0
;;

let fold_into_groups n acc_seq el () =
	match Seq.uncons acc_seq with
	| None -> Seq.Cons ([el], Seq.empty)
	| Some (l, tail) ->
		if List.length l = n
		then Seq.Cons ([el], acc_seq)
		else Seq.Cons (el :: l, tail)

let rec find_common_letter idx l =
	let letter = (List.hd l).[idx] in
	if
		List.tl l
		|> List.for_all (fun s -> String.contains s letter)
	then
		letter
	else
		find_common_letter (succ idx) l

let part2 = 
	input_file
	|> Seq.fold_left (fold_into_groups 3) Seq.empty
	|> Seq.map (find_common_letter 0)
	|> Seq.map priority
	|> Seq.fold_left (+) 0
;;

(* Final print *)

print_results (string_of_int part1) (string_of_int part2) ;;
