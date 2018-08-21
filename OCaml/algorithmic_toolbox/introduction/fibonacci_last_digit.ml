let modulus = 10


let fibonacci n =
    let rec inner k a b =
        if k < 1 then a else inner (k - 1) b ((a + b) mod modulus)
    in inner n 0 1


let () =
    let n = read_int() in
    print_int (fibonacci n);
    print_newline()
