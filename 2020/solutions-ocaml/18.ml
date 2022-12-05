type token = 
	| TNum of int
	| TLeft_Paren
	| TRight_Paren
	| TPlus
	| TTimes
;;

let zero_code = Char.code '0' ;;
let nine_code = Char.code '9' ;;

let is_digit c =
	Char.code c >= zero_code && Char.code c <= nine_code
;;

let conversion_helper acc c =
	(* Receives the accumulated stack of tokens and the next character,
	and returns the updated stack *)
	match c with
	| '*' -> TTimes :: acc
	| '+' -> TPlus :: acc
	| '(' -> TLeft_Paren :: acc
	| ')' -> TRight_Paren :: acc
	| _ when not (is_digit c) -> acc
	| _ -> 
		begin
			let value = Char.code c - zero_code in
			match acc with
			| TNum i :: t -> TNum (i * 10 + value) :: t
			| _ -> TNum value :: acc
		end
;;

let convert_to_token_stack s =
	s
	|> String.fold_left conversion_helper []
	|> List.rev
;;

let rec left_to_right_computation token_stack =
	match token_stack with
	(* Base case *)
	| TNum a :: [] -> a
	(* Simple recursion *)
	| TNum a :: TPlus :: TNum b :: t -> 
		left_to_right_computation (TNum (a + b) :: t)
	| TNum a :: TTimes :: TNum b :: t -> 
		left_to_right_computation (TNum (a * b) :: t)
