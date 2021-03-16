let () =
  let countwords = Hashtbl.create 33_000 in
  try
    while true do
      read_line () |> String.lowercase_ascii |> String.split_on_char ' '
      |> List.iter (fun token ->
             let word = String.trim token in
             if word = "" then ()
             else
               match Hashtbl.find_opt countwords word with
               | Some v -> Hashtbl.replace countwords word (v + 1)
               | None -> Hashtbl.add countwords word 1)
    done
  with End_of_file ->
    List.of_seq (Hashtbl.to_seq countwords)
    |> List.sort (fun (_, x) (_, y) -> y - x)
    |> List.iter (fun (w, c) -> Printf.printf "%s %d\n" w c)
