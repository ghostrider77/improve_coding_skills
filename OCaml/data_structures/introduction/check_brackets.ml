let opening_brackets = ['('; '['; '{']


let closing_brackets = [')'; ']' ; '}']


type opened_bracket = { bracket : char; position : int }


let do_brackets_match opening_bracket closing_bracket =
    (opening_bracket = '(' && closing_bracket = ')') ||
    (opening_bracket = '[' && closing_bracket = ']') ||
    (opening_bracket = '{' && closing_bracket = '}')


let retrieve_failed_opening_index = function
    | [] -> None
    | { position } :: _ -> Some position


let find_index_of_non_matching_bracket line =
    let length = String.length line in
    let rec loop stack ix =
        if ix = length then stack, None
        else
            let character = String.get line ix in
            if List.mem character opening_brackets then loop ({ bracket = character; position = ix } :: stack) (ix + 1)
            else if List.mem character closing_brackets then
                match stack with
                    | [] -> stack, Some ix
                    | { bracket = opening_bracket } :: stack_tail ->
                        if do_brackets_match opening_bracket character then loop stack_tail (ix + 1)
                        else stack, Some ix
            else loop stack (ix + 1) in
    let remaining_stack, failed_index = loop [] 0 in
    match failed_index with
        | None -> retrieve_failed_opening_index remaining_stack
        | _ -> failed_index


let () =
    let line = read_line () in
    match find_index_of_non_matching_bracket line with
        | None -> print_endline "Success"
        | Some ix -> print_int (ix + 1); print_newline()
