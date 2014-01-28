

let rec power_naive base exponent =
    if (=) exponent 0 then 1
    else base * (power_naive base (exponent - 1))


let rec power_repeated_squaring base exponent =
    match exponent with
        0 -> 1
        | 1 -> base
        | n ->
            let base' = base * base in
            let exponent' = exponent / 2 in (* integer division, rounding down *)
            if n mod 2 == 0 then
                (* even *)
                power_repeated_squaring base' exponent'
            else
                (* odd *)
                base * ( power_repeated_squaring base' exponent' )
                
                

let power_repeated_squaring_tailrecursive base exponent =
    let rec helper base exponent acc =
        match exponent with
            0 -> acc
            | 1 -> base * acc
            | n ->
                let base' = base * base in
                let exponent' = exponent / 2 in (* integer division, rounding down *)
                if n mod 2 == 0 then
                    (* even *)
                    helper base' exponent' acc
                else
                    (* odd *)
                    let acc' = acc * base in
                    helper base' exponent' acc'
    in
    helper base exponent 1