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
  let scores_line = read_line () in
  let scores_split = Str.split (Str.regexp " ") scores_line in
  let res = ref 0 in
  for i = 0 to n - 1 do
    let current_score = int_of_string (List.nth scores_split i) in
    if
      current_score >= int_of_string (List.nth scores_split (k - 1))
      && current_score > 0
    then res := !res + 1
  done;
  Printf.printf "%d\n" !res

let _domino_piling () =
  let mn_line = read_line () in
  let mn_split = Str.split (Str.regexp " ") mn_line in
  let m = int_of_string (List.nth mn_split 0) in
  let n = int_of_string (List.nth mn_split 1) in
  Printf.printf "%d\n" (m * n / 2)

let _bit_plusplus () =
  let n = read_int () in
  let x = ref 0 in
  for _i = 1 to n do
    let op = read_line () in
    if String.contains op '+' then x := !x + 1 else x := !x - 1
  done;
  Printf.printf "%d\n" !x

let _beautiful_matrix () =
  let find_index_of_one list =
    let rec find_helper index = function
      | [] -> failwith "No 1 found in the list"
      | hd :: tl -> if hd = 1 then index else find_helper (index + 1) tl
    in
    find_helper 0 list
  in
  let i = ref (-1) in
  let j = ref (-1) in
  for _i = 1 to 5 do
    let line = read_line () in
    let line_split = Str.split (Str.regexp " ") line in
    let line_split_int = List.map int_of_string line_split in
    if String.contains line '1' then (
      i := _i - 1;
      j := find_index_of_one line_split_int)
  done;
  let row_diff = abs (2 - !i) in
  let col_diff = abs (2 - !j) in
  let res = row_diff + col_diff in
  Printf.printf "%d\n" res

let _petya_and_strings () =
  let s1 = read_line () in
  let s2 = read_line () in
  let s1 = String.lowercase_ascii s1 in
  let s2 = String.lowercase_ascii s2 in
  let res = ref (-2) in
  if s1 < s2 then res := -1 else if s1 > s2 then res := 1 else res := 0;
  Printf.printf "%d\n" !res

let _helpful_maths () =
  let rec count_substring_occurrences str substr =
    let len_str = String.length str in
    let len_substr = String.length substr in
    if len_str < len_substr then 0
    else
      let last_chars = String.sub str (len_str - len_substr) len_substr in
      let rest_of_str = String.sub str 0 (len_str - 1) in
      let count_rest = count_substring_occurrences rest_of_str substr in
      if last_chars = substr then 1 + count_rest else count_rest
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
  let s1 = read_line () in
  let len_s1 = String.length s1 in
  let rest = String.sub s1 1 (len_s1 - 1) in
  let res = String.capitalize_ascii (String.sub s1 0 1) ^ rest in
  Printf.printf "%s\n" res

let _boy_or_girl () =
  let count_distinct_letters s =
    let seen = Hashtbl.create (String.length s) in
    let rec count_distinct_chars idx count =
      if idx >= String.length s then count
      else
        let c = String.get s idx in
        if not (Hashtbl.mem seen c) then (
          Hashtbl.add seen c true;
          count_distinct_chars (idx + 1) (count + 1))
        else count_distinct_chars (idx + 1) count
    in
    count_distinct_chars 0 0
  in
  let msg = read_line () in
  let odd_letters = count_distinct_letters msg in
  if odd_letters mod 2 = 0 then Printf.printf "CHAT WITH HER!\n"
  else Printf.printf "IGNORE HIM!\n"

let _stones_on_the_table () =
  let _n = read_int () in
  let stones = read_line () in
  let rec stones_on_the_table_helper stones count =
    if String.length stones = 1 then count
    else
      let first = String.get stones 0 in
      let second = String.get stones 1 in
      if first = second then
        stones_on_the_table_helper
          (String.sub stones 1 (String.length stones - 1))
          (count + 1)
      else
        stones_on_the_table_helper
          (String.sub stones 1 (String.length stones - 1))
          count
  in
  let res = stones_on_the_table_helper stones 0 in
  Printf.printf "%d\n" res

let _bear_and_big_brother () =
  let line = read_line () in
  let split = Str.split (Str.regexp " ") line in
  let a = int_of_string (List.nth split 0) in
  let b = int_of_string (List.nth split 1) in
  let rec bear_and_big_brother_helper a b count =
    if a > b then count
    else bear_and_big_brother_helper (a * 3) (b * 2) (count + 1)
  in
  let res = bear_and_big_brother_helper a b 0 in
  Printf.printf "%d\n" res

let _soldier_and_bananas () =
  let line = read_line () in
  let split = Str.split (Str.regexp " ") line in
  let k = int_of_string (List.nth split 0) in
  let n = int_of_string (List.nth split 1) in
  let w = int_of_string (List.nth split 2) in
  let res = ref 0 in
  for i = 1 to w do
    res := !res + (i * k)
  done;
  let res = !res - n in
  if res < 0 then Printf.printf "0\n" else Printf.printf "%d\n" res

let _elephant () =
  let x = read_int () in
  let res = ref 0 in
  if x mod 5 = 0 then res := x / 5 else res := (x / 5) + 1;
  Printf.printf "%d\n" !res

let _word () =
  let s = read_line () in
  let len_s = String.length s in
  let upper = ref 0 in
  let lower = ref 0 in
  for i = 0 to len_s - 1 do
    let c = String.get s i in
    if Char.uppercase_ascii c = c then upper := !upper + 1
    else lower := !lower + 1
  done;
  if !upper > !lower then Printf.printf "%s\n" (String.uppercase_ascii s)
  else Printf.printf "%s\n" (String.lowercase_ascii s)

let _wrong_subtraction () =
  let line = read_line () in
  let split = Str.split (Str.regexp " ") line in
  let n = int_of_string (List.nth split 0) in
  let k = int_of_string (List.nth split 1) in
  let rec wrong_subtraction_helper n k =
    if k = 0 then n
    else
      let last_digit = n mod 10 in
      if last_digit = 0 then wrong_subtraction_helper (n / 10) (k - 1)
      else wrong_subtraction_helper (n - 1) (k - 1)
  in
  let res = wrong_subtraction_helper n k in
  Printf.printf "%d\n" res

let _nearly_lucky_number () =
  let n = read_line () in
  let len_n = String.length n in
  let lucky_digits = ref 0 in
  for i = 0 to len_n - 1 do
    let c = String.get n i in
    if c = '4' || c = '7' then lucky_digits := !lucky_digits + 1
  done;
  if !lucky_digits = 4 || !lucky_digits = 7 then Printf.printf "YES\n"
  else Printf.printf "NO\n"

let _anton_and_danik () =
  let n = read_int () in
  let line = read_line () in
  let a = ref 0 in
  let d = ref 0 in
  for i = 0 to n - 1 do
    let c = String.get line i in
    if c = 'A' then a := !a + 1 else d := !d + 1
  done;
  if !a > !d then Printf.printf "Anton\n"
  else if !a < !d then Printf.printf "Danik\n"
  else Printf.printf "Friendship\n"

let _tram () =
  let n = read_int () in
  let res = ref 0 in
  let current = ref 0 in
  for _i = 1 to n do
    let line = read_line () in
    let split = Str.split (Str.regexp " ") line in
    let a = int_of_string (List.nth split 0) in
    let b = int_of_string (List.nth split 1) in
    current := !current - a + b;
    if !current > !res then res := !current
  done;
  Printf.printf "%d\n" !res

let _translation () =
  let s = read_line () in
  let t = read_line () in
  let len_s = String.length s in
  let len_t = String.length t in
  let res = ref true in
  if len_s <> len_t then res := false
  else
    for i = 0 to len_s - 1 do
      let c1 = String.get s i in
      let c2 = String.get t (len_t - i - 1) in
      if c1 <> c2 then res := false
    done;
  if !res then Printf.printf "YES\n" else Printf.printf "NO\n"

(* FIX: queue_at_the_school *)
let queue_at_the_school () =
  let line = read_line () in
  let split = Str.split (Str.regexp " ") line in
  let _n = int_of_string (List.nth split 0) in
  let t = int_of_string (List.nth split 1) in
  let line = read_line () in
  let rec queue_at_the_school_helper line t =
    if t = 0 then line
    else
      let len_line = String.length line in
      let res = ref "" in
      for i = 0 to len_line - 2 do
        let c1 = String.get line i in
        let c2 = String.get line (i + 1) in
        if c1 = 'B' && c2 = 'G' then (
          res := !res ^ "GB";
          if i = len_line - 2 then res := !res ^ "G")
        else res := !res ^ String.make 1 c1
      done;
      queue_at_the_school_helper !res (t - 1)
  in
  let res = queue_at_the_school_helper line t in
  Printf.printf "%s\n" res

(* TODO: vanya_and_fence *)

let () = queue_at_the_school ()
