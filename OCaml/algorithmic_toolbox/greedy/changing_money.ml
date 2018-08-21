let ordered_coins = [10; 5; 1]


let calc_minimum_number_of_coins amount =
    let rec inner money coins nr_changes = match coins with
        | [] -> nr_changes
        | coin :: remainingCoins -> inner (money mod coin) remainingCoins (nr_changes + money / coin)
    in inner amount ordered_coins 0


let () =
    let amount = read_int() in
    print_int (calc_minimum_number_of_coins amount);
    print_newline()
