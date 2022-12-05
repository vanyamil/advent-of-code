(*
	Year: 2022
	Day: 5
	Name: Supply Stacks
*)

(* Header *)

#use "shared/utilities.ml" ;;

let input_file = file_to_line_seq "2022/inputs/5.txt" ;;

(* Main file *)

type supply = char list list
type instr = {
	amount: int;
	from: int;
	upto: int;
}

let fold_into_groups n acc_seq el () =
	match Seq.uncons acc_seq with
	| None -> Seq.Cons ([el], Seq.empty)
	| Some (l, tail) ->
		if List.length l = n
		then Seq.Cons ([el], acc_seq)
		else Seq.Cons (el :: l, tail)
;;

let parse_supply_line line (sup, l) =
	let groups : string list = 
		line
		|> String.to_seq
		|> Seq.fold_left (fold_into_groups 4) Seq.empty
		|> Seq.map (fun l -> l |> List.rev |> List.to_seq |> String.of_seq)
		|> List.of_seq
	in
	let map_fn s =
		let ch = s.[1] in
		if ch = ' '
		then None
		else Some ch
	in
	let parsed = List.map map_fn groups in
	let connect l = function
		| None -> l
		| Some ch -> ch :: l
	in
	let first_time = function
		| None -> []
		| Some x -> [x]
	in
	let sup' = 
		if List.length sup = 0
		then List.map first_time parsed
		else List.map2 connect sup parsed
	in
	(sup', l)
;;

let parse_instr_line line (sup, l) =
	let split = String.split_on_char ' ' line in
	let instr = {
		amount = List.nth split 1 |> int_of_string;
		from = List.nth split 3 |> int_of_string |> pred;
		upto = List.nth split 5 |> int_of_string |> pred
	}
	in
	(sup, instr :: l)
;;

let parse_input (sup, l: supply * instr list) line =
	if String.length line = 0
	then (sup, l)
	else if line.[0] = '['
	then parse_supply_line line (sup, l)
	else if line.[0] = 'm'
	then parse_instr_line line (sup, l)
	else (sup, l) (* Noop *)
;;

let parsed_file =
	let (sup, l) = Seq.fold_left parse_input ([], []) input_file in
	(List.rev_map List.rev sup, List.rev l)
;;

let move (keep_order : bool) (sup: supply) (instr: instr) = 
	let from_l = List.nth sup instr.from in
	let upto_l = List.nth sup instr.upto in
	let rec extract from_l acc n =
		if n = 0 then (from_l, acc)
		else match from_l with
			| [] -> failwith "Should not be empty"
			| h :: from_l' -> extract from_l' (h :: acc) (pred n)
	in
	let (from_l, acc) = extract from_l [] instr.amount in
	let acc' = 
		if keep_order
		then List.rev acc
		else acc
	in
	let upto_l = acc' @ upto_l in
	List.mapi (fun i x -> 
		if i = instr.from
		then from_l
		else if i = instr.upto
		then upto_l
		else x
	) sup
;;

let rearrange keep_order = 
	let (sup, l) = parsed_file in
	List.fold_left (move keep_order) sup l
;;

(* Making this took an hour, holy hek *)

let part1 = 
	rearrange false
	|> List.map List.hd
	|> List.to_seq
	|> String.of_seq
;;

let part2 = 
	rearrange true
	|> List.map List.hd
	|> List.to_seq
	|> String.of_seq
;;

(* Final print *)

print_results part1 part2;;
