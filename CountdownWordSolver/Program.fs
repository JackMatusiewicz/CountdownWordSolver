namespace Countdown

module Program =

    let getLinesFromFile (path : string) = seq {
        use reader = new System.IO.StreamReader(path)
        while reader.EndOfStream = false do
            yield reader.ReadLine()
    }

    let filterInvalidWords (words : string seq) =
        let regex = System.Text.RegularExpressions.Regex("[a-z]")
        words
        |> Seq.map (fun w -> w.ToLower())
        |> Seq.filter (fun w -> w.Length > 3)
        |> Seq.filter (fun w -> regex.Match(w).Success = true)

    let getValidWordsFromFile = getLinesFromFile >> filterInvalidWords

    [<EntryPoint>]
    let main argv =
        let trie = "/usr/share/dict/words" |> getValidWordsFromFile |> Trie.buildTrie
        let foundWords =
            "fnioeihkp"
            |> List.ofSeq
            |> Permutation.findAll
            |> List.map (Trie.findAllWords trie)
            |> List.filter (fun x -> Set.isEmpty x = false)
            |> List.fold (fun acc ele -> Set.union acc ele) Set.empty
            |> Set.toList |> List.sortBy (fun x -> x.Length)
        printfn "%A" foundWords
        0
