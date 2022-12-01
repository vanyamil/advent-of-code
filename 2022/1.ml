let rec sums file_lines cur_acc total_acc =
	if file_lines = []
	then cur_acc :: total_acc
	else 
	let (h :: t) = file_lines in
	if h = ""
	then sums t 0 (cur_acc :: total_acc)
	else
	let v = int_of_string h in
	sums t (v + cur_acc) total_acc
;;

let part1 file_lines = 
	sums file_lines 0 []
	|> List.fold_left max 0
;;

let rec sublist n acc l =
	if n <= 0
	then acc
	else match l with
	| [] -> List.rev acc
	| h :: t -> sublist (n - 1) (h :: acc) t

let part2 file_lines =
	sums file_lines 0 []
	|> List.sort compare
	|> List.rev
	|> sublist 3 []
	|> List.fold_left (+) 0
;;