(*
	Year: 2022
	Day: 11
	Name: Monkey in the Middle
*)

(* Header *)

#use "shared/utilities.ml" ;;

let input_file = file_to_line_seq "2022/inputs/11-parsed.txt" ;;

(* Main file *)

type op =
	| TPlus of int
	| TTimes of int
	| TExp of int

type monke = int

type monke_rec = {
	idx: monke;
	op: op;
	modby: int;
	ontrue: monke;
	onfalse: monke;
	mutable cnt: int;
}

type recc = monke_rec * int list

let line_to_rec line = 
	let convert_op op = 
		match String.split_on_char ' ' op with
		| [op; num] -> 
			let num = int_of_string num in
			match op with
			| "+" -> TPlus num
			| "*" -> TTimes num
			| "^" -> TExp num
			| _ -> failwith "Unknown operator symbol"
		| _ -> failwith "Cannot process operation"
	in
	match String.split_on_char ':' line with
	| [idx; items; op; modby; ontrue; onfalse] ->
		let idx = int_of_string idx in
		let items = items |> String.split_on_char ',' |> List.map int_of_string in
		let op = convert_op op in
		let modby = int_of_string modby in
		let ontrue = int_of_string ontrue in
		let onfalse = int_of_string onfalse in
		let cnt = 0 in
		{ idx; op; modby; ontrue; onfalse; cnt }, items
	| _ -> failwith ("Bad line " ^ line)

let rec pow base = function
	| 0 -> 1
	| 1 -> base
	| n -> pow base (pred n) * base

let process_op item = function
	| TPlus n -> item + n
	| TTimes n -> item * n
	| TExp n -> pow item n 

let process_item monke item =
	monke.cnt <- succ monke.cnt;
	let item' = process_op item monke.op / 3 in
	if item' mod monke.modby = 0 
	then (item', monke.ontrue)
	else (item', monke.onfalse)

(* Unfortunately, have to use classic arrays here, because updating a list
	of monkeys without mutable data structure would be annoying *)

let add_back (arr : recc array) (v, to_idx) = 
	let (monke, l) = arr.(to_idx) in
	arr.(to_idx) <- (monke, l @ [v])

let rec process_monkes arr idx: unit = 
	Printf.printf "Index %d, length %d\n" idx (arr.(idx) |> snd |> List.length);
	snd arr.(idx)
	|> List.map (process_item (fst arr.(idx)))
	|> List.iter (add_back arr);
	arr.(idx) <- (fst arr.(idx), []);
	if succ idx < Array.length arr
	then process_monkes arr (succ idx)

let rec process_rounds arr round: unit = 
	flush stdout;
	Printf.printf "Round %d\n" round;
	if round > 0
	then begin
		process_monkes arr 0;
		process_rounds arr (pred round)
	end

let part1 = 
	let arr = 
		input_file
		|> Seq.map line_to_rec
		|> Array.of_seq
	in
	process_rounds arr 20;
	Array.iter (fun (monke, _) -> Printf.printf "%d: %d\n" monke.idx monke.cnt) arr
;;

let part2 = 
	0
;;

(* Final print *)

(* print_results (string_of_int part1) (string_of_int part2) ;; *)
