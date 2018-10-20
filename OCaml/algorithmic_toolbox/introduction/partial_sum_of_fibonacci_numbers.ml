let modulus = 10


let modulo a b =
    let result = a mod b in
    if result >= 0 then result
    else result + b


let calc_pisano_period modulus =
    let rec inner a b p =
        if a = 0 && b = 1 then p
        else inner b ((a + b) mod modulus) (p + 1)
    in inner 1 1 1


let calc_fibonacci_modulo n modulus =
    let rec inner k a b =
        if k < 1 then a
        else inner (k - 1) b ((a + b) mod modulus)
    in inner n 0 1


let calc_last_digit_of_partial_sum m n =
    let p = calc_pisano_period modulus in
    let last_digit_m_minus_one = modulo ((calc_fibonacci_modulo ((m + 1) mod p) modulus) - 1) modulus in
    let last_digit_n = modulo ((calc_fibonacci_modulo ((n + 2) mod p) modulus) - 1) modulus
    in modulo (last_digit_n - last_digit_m_minus_one) modulus


let () =
    let (m, n) = Scanf.scanf "%d %d" (fun x y -> (x, y)) in
    print_int (calc_last_digit_of_partial_sum m n);
    print_newline()
