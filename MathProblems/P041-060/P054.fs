module P054

open System.IO

let cardValue (c:string) =
    let v = c.[0]
    if v = 'T' then 10
    elif v = 'J' then 11
    elif v = 'Q' then 12
    elif v = 'K' then 13
    elif v = 'A' then 14
    else int v - int '0'

let countGroups (hand:string[]) (size:int) =
    hand
    |> Array.windowed size
    |> Array.filter(fun (hs) -> hs |> Array.pairwise |> Array.exists(fun (c1, c2)-> cardValue c1 <> cardValue c2) |> not)
    |> Array.length

let getFirstGroupIndex (hand:string[]) (size:int) (startIndex:int) =
    seq {startIndex..hand.Length - 1}
    |> Seq.windowed size
    |> Seq.filter(fun (hs) -> hs |> Array.pairwise |> Array.exists(fun (i1, i2)-> cardValue hand.[i1] <> cardValue hand.[i2]) |> not)
    |> Seq.head
    |> Array.head

let isFlush (hand:string[]) =
    hand
    |> Array.pairwise
    |> Array.exists(fun (c1, c2) -> c1.[1] <> c2.[1])
    |> not

let isStraight (hand:string[]) =
    hand
    |> Array.pairwise
    |> Array.exists(fun (c1, c2) -> (cardValue c1 - 1) <> cardValue c2) //remember it is sorted descending!
    |> not

let isStraightFlush (hand:string[]) =
    isStraight hand && isFlush hand

let isOnePair (hand:string[]) =
    countGroups hand 2 = 1

let isThreeOfAKind (hand:string[]) =
    countGroups hand 3 = 1 //be sure to check after checking for full house

let isFourOfAKind (hand:string[]) =
    countGroups hand 4 = 1

let isTwoPairs (hand:string[]) =
    countGroups hand 2 = 2 //be sure to check after checking for three-of-a-kind

let isFullHouse (hand:string[]) =
    countGroups hand 3 = 1 && countGroups hand 2 = 1

let getRankValue (hand:string[]) =
    if isStraightFlush hand then 9
    elif isFourOfAKind hand then 8
    elif isFullHouse hand then 7
    elif isFlush hand then 6
    elif isStraight hand then 5
    elif isThreeOfAKind hand then 4
    elif isTwoPairs hand then 3
    elif isOnePair hand then 2
    else 1

let rec compareHands (h1:string[]) (h2:string[]) =
    if h1.Length = 0 && h2.Length = 0 then 0
    else
        let cv1 = cardValue (Array.head h1)
        let cv2 = cardValue (Array.head h2)
        if cv1 > cv2 then 1
        elif cv1 < cv2 then 2
        else compareHands (Array.tail h1) (Array.tail h2)

let sortPairHand (hand:string[]) =
    let i = getFirstGroupIndex hand 2 0
    let result = [|hand.[i..i + 1] ; hand.[..i - 1] ; hand.[i + 2..] |] |> Array.concat
    result

let sortTwoPairsHand (hand:string[]) =
    let i1 = getFirstGroupIndex hand 2 0
    let i2 = getFirstGroupIndex hand 2 i1 + 2
    let result = [|hand.[i1..i1 + 1] ; hand.[i2..i2 + 1]; hand.[..i1 - 1] ; hand.[i1 + 2..i2 - 1] ; hand.[i2 + 2..] |] |> Array.concat
    result

let sortThreeOfAKindHand (hand:string[]) =
    let i = getFirstGroupIndex hand 3 0
    let result = [|hand.[i..i + 2] ; hand.[..i - 1] ; hand.[i + 3..] |] |> Array.concat
    result

let sortFourOfAKindHand (hand:string[]) =
    let i = getFirstGroupIndex hand 4 0
    let result = [|hand.[i..i + 3] ; hand.[..i - 1] ; hand.[i + 4..] |] |> Array.concat
    result

let sortHandByRank (hand:string[]) (rankValue:int)=
    if rankValue = 1 then hand //nothing
    elif rankValue = 2 then sortPairHand hand //pair
    elif rankValue = 3 then sortTwoPairsHand hand // two pairs
    elif rankValue = 4 then sortThreeOfAKindHand hand // three of a kind
    elif rankValue = 5 then hand //straight
    elif rankValue = 6 then hand //flush
    elif rankValue = 7 then hand //fullhouse
    elif rankValue = 8 then sortFourOfAKindHand hand //four of a kind
    else hand // straightflush

let solve =
        File.ReadAllLines("D:\Development\euler-files\p054_poker.txt")
        |> Array.map (fun s -> s.Split [|' '|] )
        |> Array.map (fun a -> (a.[..4] |> Array.sortByDescending cardValue, a.[5..] |> Array.sortByDescending cardValue))
        |> Array.filter (fun (h1, h2) -> 
            let r1 = getRankValue h1
            let r2 = getRankValue h2
            if r1 > r2 then true
            elif r1 < r2 then false
            else
                let sh1 = sortHandByRank h1 r1
                let sh2 = sortHandByRank h2 r2
                if compareHands sh1 sh2 = 1 then true else false)
        |> Array.length



    


