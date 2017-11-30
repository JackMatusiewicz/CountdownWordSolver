namespace Countdown

[<RequireQualifiedAccess>]
module Permutation =

    let permutations (input : 'a list) : ('a list list) =
        let rec findAllPermutations (elementsToAdd : 'a list) (acc : 'a list list) =
            match elementsToAdd with
            | [] -> acc
            | hd::tl ->
                let newPerms = addElementToPreviousLists hd acc []
                findAllPermutations tl newPerms

        and addElementToPreviousLists (element : 'a) lists listsWithElement =
            match lists with
            | hd::tl ->
                let updated = addElementToAllPositionsInList element [] hd []
                addElementToPreviousLists element tl (updated @ listsWithElement)
            | [] -> listsWithElement

        and addElementToAllPositionsInList (element : 'a)
            frontOfList backOfList (newLists : 'a list list) =
            match backOfList with
            | [] ->
                (frontOfList @ [element]) :: newLists
            | hd::tl ->
                let updated = frontOfList @ [element] @ [hd] @ tl
                addElementToAllPositionsInList element (frontOfList @ [hd]) tl (updated::newLists)

        findAllPermutations input [[]]