module Common

let rec gcd (a:int64) (b:int64) = 
    if b = 0L
    then a
    else gcd b (a % b)

let lcm (a:int64) (b:int64) =
    a * b / gcd a b