
open Batteries

type sequence = { seq : int array; len : int }


let convert_to_intlist line = List.map int_of_string Str.(line |> split (regexp " "))


let read_input_data ?(nr_sequences = 3)() =
    let rec loop acc n =
        if n = nr_sequences then List.rev acc
        else
            let length = read_int() in
            let line = read_line() in
            let s = line |> convert_to_intlist |> Array.of_list
            in loop ({seq=s; len=length} :: acc) (n + 1)
    in loop [] 0


let calc_longest_common_subsequence = function
    | [{seq=s1; len=n1}; {seq=s2; len=n2}; {seq=s3; len=n3}] ->
        let longest_path = Array.init (n1 + 1) (fun _ -> Array.init (n2 + 1) (fun _ -> (Array.make (n3 + 1) 0))) in
        for i = 0 to (n1 - 1) do
            for j = 0 to (n2 - 1) do
                for k = 0 to (n3 - 1) do
                    let updated_value =
                        if (s1.(i) = s2.(j) && s1.(i) = s3.(k)) then longest_path.(i).(j).(k) + 1
                        else List.max [longest_path.(i).(j + 1).(k + 1);
                                       longest_path.(i + 1).(j).(k + 1);
                                       longest_path.(i + 1).(j + 1).(k)] in
                    longest_path.(i + 1).(j + 1).(k + 1) <- updated_value
                done;
            done;
        done;
        longest_path.(n1).(n2).(n3)
    | _ -> assert false


let () =
    let sequences = read_input_data() in
    let result = calc_longest_common_subsequence sequences in
    print_int result;
    print_newline()
