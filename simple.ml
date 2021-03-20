module StringHash = struct
  type t = string

  let equal k1 k2 = String.equal k1 k2

  let hash (k : string) = Hashtbl.hash k
end

module StringHashtbl = Hashtbl.Make (StringHash)

let () =
  let countwords = StringHashtbl.create 33_000 in
  try
    while true do
      read_line () |> String.lowercase_ascii |> String.split_on_char ' '
      |> List.iter (function
           | "" -> ()
           | word -> (
               try incr (StringHashtbl.find countwords word)
               with Not_found -> StringHashtbl.add countwords word (ref 1)))
    done
  with
  | End_of_file ->
    StringHashtbl.fold (fun k v acc -> (k, v) :: acc) countwords []
    |> List.sort (fun (_, x) (_, y) -> Int.compare !y !x)
    |> List.iter (fun (w, c) -> Printf.printf "%s %d\n" w !c)
