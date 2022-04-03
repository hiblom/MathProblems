module P045

let tri n = n * (n + 1L) / 2L
let pent n = n * (3L * n - 1L) / 2L
let hex n = n * (2L * n - 1L)

let solve =
    (286L, 166L, 144L) |>
    Seq.unfold(fun (a, b, c) ->
        let aa = tri a
        let bb = pent b
        let cc = hex c
        if (aa = bb && aa = cc) then
            Some(Some(aa), (a + 1L, b + 1L, c + 1L))
        elif (aa <= bb && aa <= cc) then
            Some(None, (a + 1L, b, c))
        elif (bb <= aa && bb <= cc) then
            Some(None, (a, b + 1L, c))
        else
            Some(None, (a, b, c + 1L))
    ) |>
    Seq.choose(fun a -> a) |>
    Seq.head