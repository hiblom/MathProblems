module P024

let rec factorial n =
    if n = 1
    then 1
    else n * factorial (n - 1)

let removeElement (n: int) (arr:int array) =
    Array.append
        (if n > 0 then arr.[..n-1] else [||])
        (if n < (Array.length arr) - 1 then arr.[n+1..] else [||])

let rec getPermutation (n: int) (arr:int array) =
    let len = Array.length arr
    if len = 1
    then [| Array.head arr |]
    else
        let f = factorial (len - 1)
        let i = n / f
        let r = n % f
        Array.append 
            [|arr.[i]|]
            (arr |> removeElement i |> getPermutation r)

let solve =
    [|0;1;2;3;4;5;6;7;8;9|]
    |> getPermutation 999999 //zero-based!
    |> Array.fold(fun a e -> a + (int e + int '0' |> char |> string)) ""