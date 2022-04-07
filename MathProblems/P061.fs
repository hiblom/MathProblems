module P061

let isTriangular n =
    let a = 8 * n + 1
    let r = a |> float |> sqrt |> int
    a = r * r

let isSquare n =
    let r = n |> float |> sqrt |> int
    n = r * r

let isPentagonal n =
    let a = 24 * n + 1
    let r = a |> double |> sqrt |> int
    a = r * r && r % 6 = 5

let isHexagonal n =
    let a = 8 * n + 1
    let r = a |> float |> sqrt |> int
    a = r * r && r % 4 = 3

let isHeptagonal n =
    let a = 40 * n + 9
    let r = a |> double |> sqrt |> int
    a = r * r && r % 10 = 7

let isOctagonal n =
    let a = 3 * n + 1
    let r = a |> double |> sqrt |> int
    a = r * r && r % 3 = 2

let isNGon n m =
    if n = 3 then isTriangular m
    elif n = 4 then isSquare m
    elif n = 5 then isPentagonal m
    elif n = 6 then isHexagonal m
    elif n = 7 then isHeptagonal m
    elif n = 8 then isOctagonal m
    else false

let rec findNext (firstStartingDigits:int) (nextStartingDigits:int) (availableNGons: list<int>) =
    if List.length availableNGons = 1 then
        let num = nextStartingDigits * 100 + firstStartingDigits
        if isNGon availableNGons.Head num then
            Some([num])
        else
            None
    else
        seq {10..99}
        |> Seq.choose(fun a ->
            let num = nextStartingDigits * 100 + a
            seq {0..List.length availableNGons - 1}
            |> Seq.choose(fun i ->
                if isNGon availableNGons.[i] num  then
                    match findNext firstStartingDigits a (availableNGons.[..i - 1] @ availableNGons.[i + 1..]) with
                    | Some(f) -> Some([num] @ f)
                    | None -> None
                else
                    None)
            |> Seq.tryHead)
        |> Seq.tryHead
    
let find =
    let availableNGons = [3;4;5;6;7;8]
    seq {1000..9999} 
    |> Seq.filter(fun n -> n % 100 >= 10) // 3rd digit cannot be zero
    |> Seq.choose(fun num ->
        let firstStartingDigits = num / 100
        let nextStartingDigits = num % 100
        seq {0..List.length availableNGons - 1}
        |> Seq.choose(fun i ->
            if isNGon availableNGons.[i] num  then
                match findNext firstStartingDigits nextStartingDigits (availableNGons.[..i - 1] @ availableNGons.[i + 1..]) with
                | Some(f) -> Some([num] @ f)
                | None -> None
            else
                None)
        |> Seq.tryHead)
    |> Seq.head

let solve =
    find |> List.sum