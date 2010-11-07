
module Helpers = struct

  module String = struct

    (* include String *)

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

    (* include List *)

    let zip az bz =
      let rec loop zz az bz = match (az,bz) with
          ([],_)
        | (_,[]) -> zz
        | ((a::az'),(b::bz')) -> loop ((a,b)::zz) az' bz'
      in List.rev (loop [] az bz)

    let maximum = function
      | [] -> None
      | (n::ns) -> Some (List.fold_left max n ns)

    let range a b =
      let rec loop acc b =
        if b <= a
        then acc
        else loop ((b-1) :: acc) (b-1)
      in
        loop [] b

  end

end
