(*
	Year: 2022
	Day: 7
	Name: No Space Left On Device
*)

(* Header *)

#use "shared/utilities.ml" ;;

let input_file = file_to_line_seq "2022/inputs/7.txt" ;;

(* Main file *)

type instr = 
	| MoveTo of string
	| MoveUp
	| MoveToRoot
	| StartList
	| ListsFile of string * int
	| ListsDir of string

type folder = string

(* List of folders to get to this file *)
type file_rec = {
	path: folder list;
	size: int;
	name: string
}

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

(* Note - the folder list on left is inverted! *)
type parse_fold_acc = folder list * file_rec list

let parse_fold_fn ((cur_path, files): parse_fold_acc) = function
	| MoveUp -> List.tl cur_path, files
	| MoveTo name -> name :: cur_path, files
	| MoveToRoot -> ["/"], files
	| ListsFile (name, size) -> 
		let new_f = {
			path = List.rev cur_path;
			size;
			name
		}
		in
		cur_path, new_f :: files
	(* Need to set this up so that we can correctly compute folders 
	that contain nothing but another folder *)
	| ListsDir (name) -> 
		let new_f = {
			path = List.rev cur_path;
			size = 0;
			name
		}
		in
		cur_path, new_f :: files
	(* We can ignore ls entries in this exercise *)
	| _ -> cur_path, files

let parsed = 
	input_file
	|> Seq.map line_to_instr
	|> Seq.fold_left parse_fold_fn ([], [])
	|> snd

let all_folders =
	parsed
	|> List.map (fun file_rec -> file_rec.path)
	|> List.sort_uniq compare

let file_on_path path file =
	let rec equal_until_one_end a b = match a, b with
		| [], _ -> true
		| _, [] -> false
		| h1 :: t1, h2 :: t2 -> 
			if h1 <> h2 
			then false 
			else equal_until_one_end t1 t2
	in
	equal_until_one_end path file.path

let size_of_folder folder =
	parsed 
	|> List.filter (file_on_path folder)
	|> List.fold_left (fun acc file -> acc + file.size) 0

let part1 = 
	all_folders
	|> List.rev_map size_of_folder
	|> List.filter (fun size -> size < 100000)
	|> List.fold_left (+) 0
;;

let part2 = 
	let total_space = 70000000 in
	let need_unused = 30000000 in
	let need_delete = need_unused - (total_space - size_of_folder ["/"]) in
	all_folders
	|> List.rev_map size_of_folder
	|> List.sort compare
	|> List.find (fun size -> size > need_delete)
;;

(* Final print *)

print_results (string_of_int part1) (string_of_int part2) ;;
