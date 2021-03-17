// The intention behind this optimized implementation is not to re-write C# in F# (which is possible, but was not a goal)

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

let count (counters: Dictionary<string, int ref>) word =
    let mutable count = Unchecked.defaultof<int ref>
    if (counters.TryGetValue(word, &count))
    then
        incr count
    else
        counters.Add(word, ref 1)
    counters

[<EntryPoint>]
let main argv =
    readLines ()
    |> Seq.collect tokenize
    |> Seq.fold count (new Dictionary<_,_>())
    |> Seq.sortByDescending (fun kv -> !(kv.Value))
    |> Seq.iter (fun kv -> Console.WriteLine("{0} {1}", kv.Key, !(kv.Value)))
    0
