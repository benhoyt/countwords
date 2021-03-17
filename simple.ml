let () =
  let tbl = Hashtbl.create 33_000 in
  try
    let rec loop () =
      read_line ()
      |> String.lowercase_ascii
      |> String.split_on_char ' '
      |> List.iter (fun word ->
          if String.length word != 0 then
            match (Hashtbl.find_opt tbl word) with
            | Some curr -> Hashtbl.replace tbl word (curr+1)
            | None      -> Hashtbl.add tbl word 1
        );
      loop ()
    in
    loop ()
  with End_of_file ->
    Hashtbl.to_seq tbl
    |> List.of_seq
    |> List.sort (fun (_, a) (_, b) -> b - a)
    |> List.iter (fun (word, count) -> Printf.printf "%s %d\n" word count)
