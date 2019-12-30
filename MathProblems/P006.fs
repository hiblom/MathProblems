module P006

let square n = n * n

let solve =
    let sum_sq = [1L..100L] |> List.map(square) |> List.sum
    let sq_sum = [1L..100L] |> List.sum |> square
    sq_sum - sum_sq