(*
	Year: 2022
	Day: 10
	Name: Cathode-Ray Tube
*)

(* Header *)

#use "shared/utilities.ml" ;;

let input_file = file_to_line_seq "2022/inputs/10.txt" ;;

(* Main file *)

type op = 
	| Noop
	| Addx of int

let line_to_op line = 
	match String.split_on_char ' ' line with
	| [_] -> Noop
	| [_; n] -> Addx (int_of_string n)
	| _ -> failwith "Incorrect input line"

let relevant_cycles = [20; 60; 100; 140; 180; 220]

type acc = {
	test_at: int list;
	sig_str: int;
	cycle: int;
	accum_val: int;
}

let summarize acc (op: op) = 
	let cycle, accum_val = match op with
	| Noop -> acc.cycle + 1, acc.accum_val
	| Addx n -> acc.cycle + 2, acc.accum_val + n
	in
	let test_at, sig_str = 
		if List.length acc.test_at > 0 && cycle >= List.hd acc.test_at
		then List.tl acc.test_at, acc.sig_str + List.hd acc.test_at * acc.accum_val
		else acc.test_at, acc.sig_str
	in
	{ test_at; sig_str; cycle; accum_val }

let get_result acc = acc.sig_str

let part1 = 
	input_file 
	|> Seq.map line_to_op
	|> Seq.fold_left summarize {
		test_at = relevant_cycles;
		sig_str = 0;
		cycle = 0;
		accum_val = 1
	}
	|> get_result
;;

let print_single x pos = 
	let pos' = pos mod 40 in
	if abs (pos' - x) <= 1
	then print_char '#'
	else print_char '.';
	if pos' = 39 then print_newline ()

let print_crt acc op =
	let { test_at; sig_str; cycle; accum_val } = acc in
	let cycle, accum_val = match op with
	| Noop -> 
		print_single accum_val cycle;
		cycle + 1, accum_val
	| Addx n -> 
		print_single accum_val cycle;
		print_single accum_val (cycle + 1);
		cycle + 2, accum_val + n
	in
	{ test_at; sig_str; cycle; accum_val }

let part2 = 
	input_file 
	|> Seq.map line_to_op
	|> Seq.fold_left print_crt {
		test_at = relevant_cycles;
		sig_str = 0;
		cycle = 0;
		accum_val = 1
	}
	|> (fun _ -> 0)
;;

(* Final print *)

print_results (string_of_int part1) (string_of_int part2) ;;
