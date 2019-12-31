module TriangularNumberSequence

let tri_seq = (2, 1) |> Seq.unfold (fun(a, b) -> Some(b, (a + 1, a + b))) 