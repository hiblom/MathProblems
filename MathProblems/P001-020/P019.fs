module P019

let MONTH_DAYS = [| 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |]

let count_year_sundays yr day_num sunday_count =
    seq {1..12}
    |> Seq.fold(fun (d, s) e -> 
        let sunday = if d % 7 = 5 then 1 else 0
        let month_days = if e = 2 && yr % 4 = 0 then 29 else MONTH_DAYS.[e - 1]
        (d + month_days, s + sunday)) (day_num, sunday_count)

let solve =
    seq {1901..2000}
    |> Seq.fold(fun (day_num, sunday_count) e -> count_year_sundays e day_num sunday_count) (0, 0)
    |> snd

