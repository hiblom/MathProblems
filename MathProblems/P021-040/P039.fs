module P039

let countSolutions p =
    seq {
        for a in 1..p / 3 do
            for b in a..p / 2 do
                yield (a, b)} |>
    Seq.map(fun (a, b) -> (a, b, p - a- b)) |>
    Seq.filter(fun(a, b, c) -> a * a + b * b = c * c) |>
    Seq.length


let solve = 
    seq {3..1000} |>
    Seq.map(fun a -> (a, countSolutions a)) |>
    Seq.maxBy snd |>
    fst

