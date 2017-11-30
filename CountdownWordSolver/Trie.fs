namespace Countdown

open System

type WordAtNode =
    | WordEndsHere
    | NoWord

type TrieNode =
    | Node of Map<char, TrieNode> * WordAtNode

[<RequireQualifiedAccess>]
module Trie =

    let head = (Node (Map.empty, NoWord))

    let addWord (word : string) (trie : TrieNode) =
        let chars = List.ofSeq word

        let getNode (trieNode : TrieNode) (letter : char) : (TrieNode * Map<char, TrieNode>) =
            let (Node (map,_)) = trieNode
            if Map.containsKey letter map then
                (Map.find letter map, Map.remove letter map)
            else
                (Node (Map.empty, NoWord), map)

        let rec add trieNode (chars : char list) =
            let (Node (map,WordAtNode)) = trieNode
            match chars with
            | [] ->
                Node (map, WordEndsHere)
            | hd::tl ->
                let child, updatedMap = getNode trieNode hd
                let newSubTree = add child tl
                Node (Map.add hd newSubTree updatedMap, WordAtNode)
        add trie chars

    let buildTrie (words : string seq) =
        words |> Seq.fold (fun trie word -> addWord word trie) head

    let contains (word : string) (trie : TrieNode) =
        let chars = List.ofSeq word

        let getNode trieNode letter =
            let (Node (map,_)) = trieNode
            if Map.containsKey letter map then
                Map.find letter map |> Some
            else
                None

        let rec find trieNode letters =
            match letters with
            | [] ->
                match trieNode with
                | (Node (_, NoWord))       -> false
                | (Node (_, WordEndsHere)) -> true
            | hd :: tl ->
                match getNode trieNode hd with
                | Some node -> find node tl
                | None -> false
        find trie chars

    let findAllWords (trie : TrieNode) (characters : char list) : (Set<string>) =

        let rec find (currentNode : TrieNode) (processedChars : char list)
                        (toVisit : char list) (acc : Set<string>) =
            match toVisit with
            | [] -> acc
            | hd :: tl ->
                let updatedProcessedChars = hd :: processedChars
                let (Node (map, state)) = currentNode
                if Map.containsKey hd map then
                    let (Node (resultMap, newState)) as nextNode = Map.find hd map
                    if newState = WordEndsHere then
                        let newWord =
                            updatedProcessedChars
                            |> List.rev
                            |> Array.ofList
                            |> String
                        find nextNode updatedProcessedChars tl (Set.add newWord acc)
                    else find nextNode updatedProcessedChars tl acc
                else acc
        find trie [] characters Set.empty