let rec calc_gcd a b =
    if b = 0 then a else calc_gcd b (a mod b)


let () =
    let (a, b) = Scanf.scanf "%d %d" (fun x y -> (x, y)) in
    print_int (calc_gcd a b);
    print_newline()
