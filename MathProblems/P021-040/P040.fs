module P040

//careful, index is 1-based in the problem

let traverse (n, wantedIndex, currentIndex) =
    if wantedIndex > 999999
        then None
    else
        let ns = string n
        let len = String.length ns
        if currentIndex + len > wantedIndex then
            let decimal = int ns.[wantedIndex - currentIndex] - int '0'
            Some((Some(decimal)),(n + 1, wantedIndex * 10 + 9, currentIndex + len))
        else
            Some((None),(n + 1, wantedIndex, currentIndex + len))

let solve =
    (1, 0, 0) |>
    Seq.unfold traverse |>
    Seq.choose (fun a -> a) |>
    Seq.reduce ((*))

