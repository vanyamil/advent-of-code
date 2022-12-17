(*
	Year: 2022
	Day: 15
	Name: Beacon Exclusion Zone
*)

(* Header *)

#use "shared/utilities.ml" ;;

let input_file = file_to_line_seq "2022/inputs/15.txt" ;;

(* Main file *)

type info = {
	sensor: V.t;
	beacon: V.t;
}

let line_to_info = 
	let line_fmt = format_of_string "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
	in
	fun line ->
	Scanf.sscanf line line_fmt (fun sx sy bx by -> {
		sensor = (sx, sy);
		beacon = (bx, by)
	})

(* Does this sensor cover up at y, and if so, how much? *)
let coverup y info = 
	let d = V.manhattan info.sensor info.beacon in
	let (sx, sy) = info.sensor in
	let vert = abs (y - sy) in
	if vert > d
	then None
	else 
	let split_dist = d - vert in
	Some (sx - split_dist, sx + split_dist)
	(* The remainder of distance at point (sx, y) "splits" in both horizontal directions, 
		+ that point itself *)

let rec merge_segs (a1, a2) (b1, b2) = 
	if b1 < a1 (* Make sure seg a lies on left of seg b *)
	then merge_segs (b1, b2) (a1, a2)
	else if a2 < b1 (* a1 a2 b1 b2 *)
	then None
	else (*  *)
	Some (a1, max a2 b2)

let rec fold_segments seglist seg = match seglist with
	| [] -> [seg]
	| h :: t -> 
		match merge_segs h seg with
		| None -> h :: fold_segments t seg
		| Some mseg -> fold_segments t mseg

let count_dist sum (x1, x2) = sum + (x2 - x1 + 1)

let count_beacons_at_y y' (cnt, found_xs) info = 
	let (x, y) = info.beacon in
	if y = y' && not (List.mem x found_xs)
	then (succ cnt, x :: found_xs)
	else (cnt, found_xs)

let part1 = 
	let infos = Seq.map line_to_info input_file in
	let y = 2000000 in
	let beacons_at_y = infos |> Seq.fold_left (count_beacons_at_y y) (0, []) |> fst in
	infos
	|> Seq.filter_map (coverup y)
	|> Seq.fold_left fold_segments []
	|> List.fold_left count_dist (~- beacons_at_y)
;;

let part2 = 
	0
;;

(* Final print *)

(* print_results (string_of_int part1) (string_of_int part2) ;; *)
