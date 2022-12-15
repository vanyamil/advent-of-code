(*
	Year: 2022
	Day: 13
	Name: Distress Signal
*)

(* Header *)

#use "shared/utilities.ml" ;;

let input_file = file_to_line_seq "2022/inputs/13.txt" ;;

(* Main file *)

(* 3 steps - lexer, parser, comparison *)

(* Lexer *)

type token =
	| StartList
	| EndList
	| Value of int
	| ListSep

let isdigit c = 
	c >= '0' && c <= '9'

let todigit c = 
	Char.code c - Char.code '0'

let lex_tokens acc c =
	if c = '['
	then StartList :: acc
	else if c = ']'
	then EndList :: acc
	else if c = ','
	then ListSep :: acc
	else if isdigit c
	then
	match acc with
	| Value n :: t -> Value (n * 10 + todigit c) :: t
	| _ -> Value (todigit c) :: acc
	else
	failwith "Unknown symbol"

let line_to_opt_tokens linestr: token list option = 
	if linestr = "" 
	then None
	else
	Some (linestr |> String.fold_left lex_tokens [] |> List.rev)

(* Parser *)

type element = 
	| EValue of int
	| EList of element list

let rec parse_element: token list -> element * token list = function
	| Value n :: t -> EValue n, t
	| StartList :: t -> 
	begin
		let (l, rem) = parse_list [] t in
		match rem with
		| EndList :: t -> EList l, t
		| _ -> failwith "Failed detecting end list when parsing list element"
	end
	| _ -> failwith "Failed parsing an element"
and parse_list acc l : element list * token list = match l with
	| EndList :: t -> (List.rev acc), l
	| Value _ :: t
	| StartList :: t -> 
	begin
		let (el, rem) = parse_element l in
		match rem with
		| ListSep :: t -> parse_list (el :: acc) t
		| _ -> parse_list (el :: acc) rem
	end
	| _ -> failwith "Failed parsing a list"

let parse_tokens l = 
	let (el, rem) = parse_element l in
	match rem with
	| [] -> el
	| _ -> failwith "Somehow didn't parse whole expr"

let parsed_input = 
	input_file
	|> Seq.filter_map line_to_opt_tokens
	|> Seq.map parse_tokens

(* Comparator *)

let rec ecompare e1 e2 onequal = match e1, e2 with
	| EValue a, EValue b -> 
		let c = compare a b in
		if c = 0
		then onequal ()
		else c
	| EValue _, _ -> ecompare (EList ([e1])) e2 onequal
	| _, EValue _ -> ecompare e1 (EList ([e2])) onequal
	| EList [], EList [] -> onequal ()
	| EList _, EList [] -> 1
	| EList [], EList _ -> -1
	| EList (ah :: at), EList (bh :: bt) ->
		ecompare ah bh (fun () -> ecompare (EList at) (EList bt) onequal)

let ecompare e1 e2 = ecompare e1 e2 (fun () -> 0)

(* Task *)

let count_right_orders acc idx (lhs, rhs) =
	if ecompare lhs rhs <= 0
	then acc + idx + 1 (* Result needs to be 1-indexed *)
	else acc

let pair_up (last_opt, acc) entry = match last_opt with
	| None -> Some entry, acc
	| Some first -> None, Seq.append acc (Seq.return (first, entry))

let part1 = 
	parsed_input
	|> Seq.fold_left pair_up (None, Seq.empty)
	|> snd
	|> Seq.fold_lefti count_right_orders 0
;;

let mult_if_divider div1 div2 (idx, prod) entry =
	if entry = div1 || entry = div2
	then (succ idx, prod * idx)
	else (succ idx, prod)

let part2 = 
	let div1 = EList [EList [EValue 2]] in
	let div2 = EList [EList [EValue 6]] in
	parsed_input
	|> Seq.cons div1
	|> Seq.cons div2
	|> List.of_seq
	|> List.sort ecompare
	|> List.fold_left (mult_if_divider div1 div2) (1, 1)
	|> snd
;;

(* Final print *)

print_results (string_of_int part1) (string_of_int part2) ;;
