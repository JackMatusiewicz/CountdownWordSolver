module Permutation

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

//A more concise way to do it - but much slower.
let addInAllPositions (element : 'a) (data : 'a list) =
    let rec addValueInAllPositions (element : 'a)
        frontOfList backOfList (newLists : 'a list list) =
        match backOfList with
        | [] ->
            (frontOfList @ [element]) :: newLists
        | hd::tl as l ->
            let updated = frontOfList @ [element] @ l
            addValueInAllPositions element (frontOfList @ [hd]) tl (updated::newLists)
    addValueInAllPositions element [] data []

let rec perms (data : 'a list) =
    match data with
    | [] -> []
    | x::[] -> [[x]]
    | hd::tl -> List.fold (fun acc p -> acc @ addInAllPositions hd p ) [] (perms tl)