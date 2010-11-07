
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

  let zip az bz =
    let rec loop zz az bz = match (az,bz) with
        ([],_)
      | (_,[]) -> zz
      | ((a::az'),(b::bz')) -> loop ((a,b)::zz) az' bz'
    in rev (loop [] az bz)

  (* let zip_tail az bz = *)
  (*   let rec loop zz az bz = match (az,bz) with *)
  (*       ([],_) *)
  (*     | (_,[]) -> zz *)
  (*     | ((a::az'),(b::bz')) -> loop (zz @ [(a,b)]) az' bz' *)
  (*   in loop [] az bz *)

end
