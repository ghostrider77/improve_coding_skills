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


let calc_huge_fibonacci_modulo n modulus =
    let p = calc_pisano_period modulus
    in calc_fibonacci_modulo (n mod p) modulus


let () =
    let (n, modulus) = Scanf.scanf "%d %d" (fun x y -> (x, y)) in
    print_int (calc_huge_fibonacci_modulo n modulus);
    print_newline()
