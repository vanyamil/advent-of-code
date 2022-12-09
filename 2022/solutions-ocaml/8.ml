(*
	Year: 2022
	Day: 8
	Name: Treetop Tree House
*)

(* Header *)

#use "shared/utilities.ml" ;;

let input_file = file_to_line_seq "2022/inputs/8.txt" ;;

(* Main file *)

module Tensor = struct
	type t = int list * string

	let get coords (stride, s: t) = 
		let rec aux idx = function
			| [] -> s.[idx]
			| (coord, stride) :: t -> aux (idx + coord * stride) t
		in
		aux 0 (List.map2 (fun a b -> a, b) coords stride)

	let create shape s =
		let folder el (hd, tl) = (hd * el, hd :: tl) in
		let (total, stride) = List.fold_right folder shape (1, []) in
		assert (String.length s = total);
		stride, s
end

let test =
	let t = Tensor.create [3; 2] "abcdef" in
	assert (Tensor.get [0; 0] t = 'a');
	assert (Tensor.get [0; 1] t = 'b');
	assert (Tensor.get [2; 0] t = 'e')

let size = 99

let input_t =
	let shape = [size; size] in
	input_file
	|> Seq.fold_left (^) ""
	|> Tensor.create shape

let test_v start step (r, c) =
	let v = Tensor.get [r; c] input_t in
	Seq.init size (fun i -> fst start + fst step * i, snd start + snd step * i)
	|> Seq.take_while (fun t -> t <> (r, c))
	|> Seq.for_all (fun (r', c') -> Tensor.get [r'; c'] input_t < v)

let debug_test_v start step stop =
	let b = test_v start step stop in
	if b then Printf.printf "%d, %d -> %d, %d (%d): %s\n" 
		(fst start) (snd start) 
		(fst stop) (snd stop) (Char.code (Tensor.get [fst stop; snd stop] input_t) - Char.code '0')
		(if b then "T" else "F");
	b

let is_visible r c =
	test_v (0, c) (1, 0) (r, c)
	|| test_v (r, 0) (0, 1) (r, c)
	|| test_v (size - 1, c) (-1, 0) (r, c)
	|| test_v (r, size - 1) (0, -1) (r, c)

let part1 = 
	let s () = Seq.init size (fun i -> i) in
	let s2 = Seq.product (s ()) (s ()) in
	Seq.fold_left (fun cnt (r, c) -> if is_visible r c then succ cnt else cnt) 0 s2
;;

let sees_trees step (r, c) =
	let v = Tensor.get [r; c] input_t in
	let cnt = Seq.init size (fun i -> r + fst step * succ i, c + snd step * succ i)
	|> Seq.take_while (
		fun (r', c') -> r' >= 0 && c' >= 0 && r' < size && c' < size 
			&& Tensor.get [r'; c'] input_t < v
	)
	|> Seq.length
	in
	(* Here, we need to figure out if we reached limit (and thus saw all trees) 
		or reached big tree (and need to add it)
	*)
	let (r', c') = (r + fst step * cnt, c + snd step * cnt) in
	let important = if fst step = 0 then c' else r' in
	if important = 0 || important = size - 1
	then cnt
	else succ cnt

let scenic_score r c =
	sees_trees (1, 0) (r, c)
	* sees_trees (0, 1) (r, c)
	* sees_trees (-1, 0) (r, c)
	* sees_trees (0, -1) (r, c)

let part2 = 
	let s () = Seq.init size (fun i -> i) in
	let s2 = Seq.product (s ()) (s ()) in
	Seq.fold_left (fun mx (r, c) -> 
		let v = scenic_score r c in
		if v > mx then v else mx
	) 0 s2
;;

(* Final print *)

print_results (string_of_int part1) (string_of_int part2) ;;
