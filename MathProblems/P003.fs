module P003

let rec find_largest_prime_factor (n: int64): int64 =
    let sq = double n |> sqrt |> int
    if n % int64 sq = 0L
    then int64 sq
    else
        let f = seq { 2..sq } |> Seq.takeWhile(fun i -> n % int64 i <> 0L) |> Seq.last |> (+) 1
        if f = sq + 1
        then n
        else find_largest_prime_factor (n / int64 f)

let solve =
    600851475143L |> find_largest_prime_factor