open System
open System.Collections.Generic

let rec readLines () = seq {
    let line = Console.ReadLine()
    if line <> null then
        yield line
        yield! readLines ()
}

let tokenize (line: string) =
    line.ToLower().Split(' ', StringSplitOptions.RemoveEmptyEntries)

let count (counters: Dictionary<string, int>) word =
    counters.[word] <- counters.GetValueOrDefault(word, 0) + 1
    counters

[<EntryPoint>]
let main argv =
    readLines ()
    |> Seq.collect tokenize
    |> Seq.fold count (new Dictionary<_,_>())
    |> Seq.sortByDescending (fun kv -> (kv.Value))
    |> Seq.iter (fun kv -> Console.WriteLine("{0} {1}", kv.Key, (kv.Value)))
    0
