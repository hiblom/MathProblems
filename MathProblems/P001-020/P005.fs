module P005

open Common

let solve =
    seq { 1L..20L }
    |> Seq.fold (fun a e -> lcm a e) 1L