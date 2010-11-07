
type regex =
    Empty
  | Singleton of char
  | Kleene of regex
  | Catenation of regex * regex
  | Alternation of regex * regex ;;

let rec string_of_regex r = match r with
    Empty -> ""
  | Singleton c -> Char.escaped c
  | Kleene r -> (string_of_regex r) ^ "*"
  | Catenation (r1, r2) -> (string_of_regex r1) ^ (string_of_regex r2)
  | Alternation (r1, r2) -> (string_of_regex r1) ^ "|" ^ (string_of_regex r2) ;;

(* let rec rmatch auto = function *)
(*   | ''      -> auto.accepting *)
(*   |  -> rmatch {(auto.transition hd); tl} ;; *)

(* let fold_left_str =  *)

module String = struct
  
  include String

  let fold_right_str f c s =
    let rec loop acc i =
      if i < 0
      then
        acc
      else
        loop (f s.[i] acc) (i - 1)
    in loop c ((String.length s) - 1) ;;

end

module List = struct

  include List
    
  let rec zip az bz = match (az,bz) with
      ([],_)
    | (_,[]) -> []
    | ((a::az'),(b::bz')) -> (a,b) :: (zip az' bz')


  let zip_tail az bz =
    let rec loop zz 

end
