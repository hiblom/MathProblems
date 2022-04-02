module P067

open System.IO

let get_next_row (triangle: int[][]) row i =
    row 
    |> Array.pairwise
    |> Array.mapi(fun j (a, b) -> triangle.[i].[j] + max a b)

let solve =
    let triangle =
        File.ReadAllText("D:\Development\euler-files\p067_triangle.txt")
        |> fun s -> s.Split('\n')
        |> Array.filter(fun s -> s.Length > 0)
        |> Array.map(fun s -> s.Split(' ') |> Array.map(int))

    (99, triangle.[99])
    |> Seq.unfold(fun (i, row) -> 
        if i > 0 
        then
            let next_row = get_next_row triangle row (i - 1)
            Some((i - 1, next_row), (i - 1, next_row)) 
        else None)
    |> Seq.last
    |> fun (i, row) -> row.[0]