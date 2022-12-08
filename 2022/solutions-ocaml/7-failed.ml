(*
	Year: 2022
	Day: 7
	Name: No Space Left On Device
*)

(* Header *)

#use "shared/utilities.ml" ;;

let input_file = file_to_line_seq "2022/inputs/7.txt" ;;

type filename = string
type fs = File of (filename * int) | Dir of (filename * fs list)

type instr = 
	| MoveTo of string
	| MoveUp
	| MoveToRoot
	| StartList
	| ListsFile of filename * int
	| ListsDir of filename

let name = function
	| File (name, _) 
	| Dir (name, _) -> name

let rec fold_files f acc fsl = 
	let aux acc = function
		| File (name, size) -> f acc name size
		| Dir (name, fsl') -> fold_files f acc fsl'
	in
	List.fold_left aux acc fsl

let rec list_dirs = function
	| File _ -> []
	| Dir (name, fsl) as x -> 
		let l = List.concat_map list_dirs fsl in
		x :: l

let find_dirs_below_limit limit fs =
	let counter acc _ size = 
		acc + size
	in
	list_dirs fs 
	|> List.map (fun fs -> (name fs, fold_files counter 0 [fs]))
	|> List.filter (fun (_, size) -> size < limit)

let suffix s n =
	String.sub s n (String.length s - n)

let line_to_instr line =
	if line = "$ cd /"
	then MoveToRoot
	else if line = "$ cd .."
	then MoveUp
	else if line = "$ ls"
	then StartList
	else if line.[0] = '$'
	then MoveTo (suffix line 5)
	else if line.[0] = 'd'
	then ListsDir (suffix line 4)
	else
	let [size; name] = String.split_on_char ' ' line in
	ListsFile (name, int_of_string size)

(* Acc is two-fold 
	First part is the tree we build up
	Second is a continuation we call to move up
*)
type gen_acc = (fs * (fs -> fs))

let cont_generate (fs, cont) instr = 
	match fs with
	| File _ -> failwith "Should never have file at this point"
	| Dir (here, fsl) -> match instr with
		| ListsDir (name) -> 
			let new_dir = Dir (name, []) in
			(Dir (here, new_dir :: fsl), cont)
		| ListsFile (name, size) ->
			let new_f = File (name, size) in
			(Dir (here, new_f :: fsl), cont)
		| StartList ->
		| MoveToRoot ->
			(fs, cont) (* I think no-ops? *)
		| MoveUp -> 
		| MoveTo (name) -> failwith "Did not complete"

let input_fs =
	input_file
	|> Seq.map line_to_instr
	|> Seq.fold_left cont_generate (fun fs -> fs)

(* Main file *)

let part1 = 
	0
;;

let part2 = 
	0
;;

(* Final print *)

(* print_results (string_of_int part1) (string_of_int part2) ;; *)
