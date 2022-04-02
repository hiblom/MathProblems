module P036

let isPalindromicInBase n b =
    if n % b = 0 
    then false
    else
        let digits = List.unfold(fun a -> if a > 0 then Some(a % b, a / b) else None ) n 
    
        {0..digits.Length / 2 - 1} |> 
        Seq.exists(fun i -> digits.Item i <> digits.Item (digits.Length - 1 - i)) 
        |> not

let solve =
    {1..999999} |>
    Seq.filter (fun n -> (isPalindromicInBase n 2) && (isPalindromicInBase n 10)) |>
    Seq.sum