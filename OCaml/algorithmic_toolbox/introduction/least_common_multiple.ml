let rec calc_gcd a b =
    if b = 0 then a else calc_gcd b (a mod b)


let calc_lcm a b =
    let gcd = calc_gcd a b in
        (a / gcd) * b


let () =
    let (a, b) = Scanf.scanf "%d %d" (fun x y -> (x, y)) in
        print_int (calc_lcm a b);
        print_newline()
