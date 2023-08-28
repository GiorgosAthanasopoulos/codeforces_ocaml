let _watermelon () =
  let w = read_int () in
  if w <= 2 then Printf.printf "NO\n"
  else if w mod 2 = 0 then Printf.printf "YES\n"
  else Printf.printf "NO\n"

let _way_too_long_words () =
  let word_count = read_int () in
  let words = ref [] in
  for _i = 1 to word_count do
    let word = read_line () in
    words := !words @ [ word ]
  done;
  for i = 0 to word_count - 1 do
    let word = List.nth !words i in
    let length = String.length word in
    if length <= 10 then Printf.printf "%s\n" word
    else
      let first = String.get word 0 in
      let last = String.get word (length - 1) in
      Printf.printf "%c%d%c\n" first (length - 2) last
  done

let _team () =
  let problem_count = read_int () in
  let res = ref 0 in
  for _i = 1 to problem_count do
    let count = ref 0 in
    let line = read_line () in
    let split = Str.split (Str.regexp " ") line in
    for j = 0 to 2 do
      let number = int_of_string (List.nth split j) in
      count := !count + number
    done;
    if !count >= 2 then res := !res + 1
  done;
  Printf.printf "%d\n" !res

let _next_round () =
  let nk_line = read_line () in
  let nk_split = Str.split (Str.regexp " ") nk_line in
  let n = int_of_string (List.nth nk_split 0) in
  let k = int_of_string (List.nth nk_split 1) in
  let scores_line = read_line() in
  let scores_split = Str.split (Str.regexp " ") scores_line in
  let res = ref 0 in
  for i = 0 to n - 1 do
    let current_score = int_of_string (List.nth scores_split i) in
    if current_score >= int_of_string (List.nth scores_split (k - 1)) && current_score > 0
    then res := !res + 1
  done;
  Printf.printf "%d\n" !res

let _domino_piling () =
  let mn_line = read_line () in
  let mn_split = Str.split (Str.regexp " ") mn_line in
  let m = int_of_string (List.nth mn_split 0) in
  let n = int_of_string (List.nth mn_split 1) in
  Printf.printf "%d\n" ((m * n) / 2)

let _bit_plusplus () =
  let n = read_int () in
  let x = ref 0 in
  for _i = 1 to n do
    let op = read_line () in
    if String.contains op '+'
    then x := !x + 1
    else x := !x - 1
  done;
  Printf.printf "%d\n" !x

let _beautiful_matrix () =
  let find_index_of_one list =
    let rec find_helper index = function
      | [] -> failwith "No 1 found in the list"
      | hd :: tl ->
         if hd = 1 then index
         else find_helper (index + 1) tl
    in
    find_helper 0 list
  in
  let i = ref (-1) in
  let j = ref (-1) in
  for _i = 1 to 5 do
    let line = read_line () in
    let line_split = Str.split (Str.regexp " ") line in
    let line_split_int = List.map int_of_string line_split in
    if (String.contains line '1')
    then
      begin
      i := _i - 1;
      j := find_index_of_one line_split_int;
      end
  done;
  let row_diff = abs (2 - !i) in
  let col_diff = abs (2 - !j) in
  let res = row_diff + col_diff in
  Printf.printf "%d\n" res

let _petya_and_strings () =
  let s1 = read_line () in
  let s2 = read_line () in
  let s1 = String.lowercase s1 in
  let s2 = String.lowercase s2 in
  let res = ref (-2) in
  if s1 < s2
  then res := -1
  else if s1 > s2
  then res := 1
  else res := 0;
  Printf.printf "%d\n" !res

let _helpful_maths () =
  let rec count_substring_occurrences str substr =
    let len_str = String.length str in
    let len_substr = String.length substr in
    if len_str < len_substr
    then 0
    else
      let last_chars = String.sub str (len_str - len_substr) len_substr in
      let rest_of_str = String.sub str 0 (len_str - 1) in
      let count_rest = count_substring_occurrences rest_of_str substr in
      if last_chars = substr
      then 1 + count_rest
      else count_rest
  in
  let expr = read_line () in
  let ones = count_substring_occurrences expr "1" in
  let twos = count_substring_occurrences expr "2" in
  let threes = count_substring_occurrences expr "3" in
  let res = ref "" in
  for _i = 1 to ones do
    res := !res ^ "1+"
  done;
  for _i = 1 to twos do
    res := !res ^ "2+"
  done;
  for _i = 1 to threes do
    res := !res ^ "3+"
  done;
  let res_length = String.length !res in
  res := String.sub !res 0 (res_length - 1);
  Printf.printf "%s\n" !res

let _word_capitalization () =
  let char_to_bytes c =
    let bytes = Bytes.create 1 in
    Bytes.set bytes 0 c;
    bytes
  in
  let s1 = read_line () in
  let first_letter = String.get s1 0 in
  let first_letter_bytes = char_to_bytes first_letter in
  let len_s1 = String.length s1 in
  let rest = String.sub s1 1 (len_s1 - 1) in
  let res = (String.uppercase first_letter_bytes) ^ rest in
  Printf.printf "%s\n" res

let boy_or_girl () =
  let count_distinct_letters s =
    let seen = Hashtbl.create (String.length s) in
    let rec count_distinct_chars idx count =
      if idx >= String.length s
      then count
      else
        let c = String.get s idx in
        if not (Hashtbl.mem seen c)
        then begin
            Hashtbl.add seen c true;
            count_distinct_chars (idx + 1) (count + 1)
          end
        else
          count_distinct_chars (idx + 1) count
    in
  count_distinct_chars 0 0
  in
  let msg = read_line () in
  let odd_letters = count_distinct_letters msg in
  if (odd_letters mod 2 = 0)
  then Printf.printf "CHAT WITH HER!\n"
  else Printf.printf "IGNORE HIM!\n"

let () = boy_or_girl ()
