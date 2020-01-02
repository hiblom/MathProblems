module P015

let next_row row =
    [1L] @  (row |> List.pairwise |> List.map(fun (a, b) -> a + b)) @ [1L]

let solve =
    [1L]
    |> List.unfold(fun a -> 
        if a |> List.length > 41 
        then None 
        else Some(a, next_row a))
    |> List.last
    |> List.item 20