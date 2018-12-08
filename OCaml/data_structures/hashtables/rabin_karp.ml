open Batteries

let modulo a p =
    let result = a mod p in
    if result >= 0 then result
    else result + p


let polynomial_hashing s prime x =
    String.fold_right (fun chr acc -> (acc * x + Char.code chr) mod prime) s 0


let calc_power_of_x x prime exponent =
    let rec loop acc k =
        if k = exponent then acc
        else loop ((acc * x) mod prime) (k + 1)
    in loop 1 0


let calc_substring_hashes text text_length pattern_length prime x =
    let length = text_length - pattern_length + 1 in
    let hash_values = Array.make length 0 in
    let last_substring = String.slice ~first:(length - 1) text in
    hash_values.(length - 1) <- polynomial_hashing last_substring prime x;
    let x_power = calc_power_of_x x prime pattern_length in
    let code_at_position str k = Char.code (String.get str k) in
    for ix = (length - 2) downto 0 do
        let hashed = x * hash_values.(ix + 1) +
            code_at_position text ix - x_power * code_at_position text (ix + pattern_length) in
        hash_values.(ix) <- modulo hashed prime
    done;
    hash_values


let get_matching_indices text text_length pattern pattern_length pattern_hash substring_hashes =
    let length = text_length - pattern_length + 1 in
    let rec loop acc ix =
        if ix = length then List.rev acc
        else if pattern_hash = substring_hashes.(ix) &&
                pattern = String.slice ~first:ix ~last:(ix + pattern_length) text then loop (ix :: acc) (ix + 1)
        else loop acc (ix + 1)
    in loop [] 0


let find_pattern_in_text text pattern prime =
    let () = Random.init 2112 in
    let x = 1 + Random.int (prime - 2) in
    let pattern_length = String.length pattern in
    let text_length = String.length text in
    let pattern_hash_value = polynomial_hashing pattern prime x in
    let substring_hash_values = calc_substring_hashes text text_length pattern_length prime x
    in get_matching_indices text text_length pattern pattern_length pattern_hash_value substring_hash_values


let () =
    let pattern = read_line() in
    let text = read_line () in
    let prime = 1000000007 in
    let indices = find_pattern_in_text text pattern prime in
    List.iter (function elem -> print_int elem; print_string " ") indices;
    print_newline()
