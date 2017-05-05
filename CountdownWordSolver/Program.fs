    // Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Trie

let getLinesFromFile (path : string) = seq {
    use reader = new System.IO.StreamReader(path)
    while reader.EndOfStream = false do
        yield reader.ReadLine()
}

let filterInvalidWords (words : string seq) =
    let regex = System.Text.RegularExpressions.Regex("[a-z]")
    words
        |> Seq.map (fun w -> w.ToLower())
        |> Seq.filter (fun w -> regex.Match(w).Success)

let getValidWordsFromFile = getLinesFromFile >> filterInvalidWords

[<EntryPoint>]
let main argv =
    let trie = "/usr/share/dict/words" |> getValidWordsFromFile |> Trie.buildTrie

    printfn "%A" <| Trie.contains "accenture" trie


    0 // return an integer exit code
