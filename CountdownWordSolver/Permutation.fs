module Permutation

let permutations (input : 'a list) : ('a list list) =
    let rec findAllPermutations (elementsToAdd : 'a list) (acc : 'a list list) =
        match elementsToAdd with
        | [] -> acc
        | hd::tl ->
            let newPerms = addElementToPreviousLists hd acc []
            findAllPermutations tl newPerms

    and addElementToPreviousLists (element : 'a) lists updatedLists =
        match lists with
        | hd::tl ->
            let updated = addElementToAllPositionsInList element [] hd []
            addElementToPreviousLists element tl (updated @ updatedLists)
        | [] -> updatedLists

    and addElementToAllPositionsInList (element : 'a) frontOfList backofList (newLists : 'a list list) =
        match backofList with
        | [] ->
            [(frontOfList @ [element])] @ newLists
        | hd::tl ->
            let updated = frontOfList @ [element] @ [hd] @ tl
            addElementToAllPositionsInList element (frontOfList @ [hd]) tl (updated::newLists)

    findAllPermutations input [[]]