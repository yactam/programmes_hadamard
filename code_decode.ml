module CharMap = Map.Make(Char)

let rec sylvester (n : int) : int array array =
  if n = 0 then [|[|1|]|]
  else
    let prev = sylvester (n - 1) in
    let size = 1 lsl (n - 1) in
    let matrix = Array.make_matrix (2 * size) (2 * size) 0 in
    for i = 0 to size - 1 do
      for j = 0 to size - 1 do
        matrix.(i).(j) <- prev.(i).(j);
        matrix.(i).(j + size) <- prev.(i).(j);
        matrix.(i + size).(j) <- prev.(i).(j);
        matrix.(i + size).(j + size) <- -prev.(i).(j);
      done;
    done;
    matrix;;

let hadamard_code alphabet =
  let l = String.length alphabet in
  let k = int_of_float (ceil (log(float_of_int l) /. log(2.))) in
  let hadamard_matrix = sylvester k in
  let code = Array.make_matrix l (1 lsl k) 0 in

  for i = 0 to l - 1 do
    let row = hadamard_matrix.(i) in
    for j = 0 to (1 lsl k) - 1 do
      code.(i).(j) <- row.(j)
    done
  done;

  let code_map = ref CharMap.empty in
  for i = 0 to l - 1 do
    let letter = String.get alphabet i in
    let letter_code = Array.to_list code.(i) in
    code_map := CharMap.add letter letter_code !code_map;
  done;

  !code_map
;;

let rec sublist s l lst = 
  let rec aux i acc = function
    | [] -> List.rev acc
    | h :: t ->
      if i < s then aux (i + 1) acc t
      else if i < s + l then aux (i + 1) (h :: acc) t
      else List.rev acc
  in
  aux 0 [] lst

let find_index pred arr =
  let result = ref None in
  Array.iteri (fun i x -> if pred x then result := Some i) arr;
  !result

let decode_message coded_message code_map =
  let l = List.length coded_message in
  let k = 
    match CharMap.choose_opt code_map with
    | Some (_, code) -> List.length code
    | None -> failwith "Code vide!"
  in
  let order = int_of_float (ceil (log(float_of_int k) /. log(2.))) in
  let hadamard_matrix = sylvester order in

  let rec decode_word start_index =
    if start_index < l then begin
      let word_code = Array.of_list (sublist start_index k coded_message) in
      let result_vector = Array.make k 0 in

      for i = 0 to k - 1 do
        let row = hadamard_matrix.(i) in
        for j = 0 to k - 1 do        
          result_vector.(i) <- result_vector.(i) + (word_code.(j) * row.(j))
        done;
      done;

      let max_value = Array.fold_left max result_vector.(0) result_vector in
      let letter_index =  find_index (fun x -> x = max_value) result_vector in
      let letter_index =
        match letter_index with
        | None   -> failwith "Lettre invalide !"
        | Some i -> i 
      in 
      let letter = ref None in
      let code_list = CharMap.to_list code_map in
      List.iteri (fun index (l, _) ->
        if index = letter_index then letter := Some l
      ) code_list;

      match !letter with
      | Some c -> c :: decode_word (start_index + k)
      | None   -> '?' :: decode_word (start_index + k)
    end else
      []
  in

  decode_word 0
;;
 
let () =
  print_string "Veuillez saisir l'alphabet: ";
  let alphabet = read_line () in
  let code = hadamard_code alphabet in
  CharMap.iter (fun letter code -> 
    Printf.printf "%c: %s\n" letter 
    (String.concat " " (List.map string_of_int code))) code;
    
  print_endline "Entrez le message à coder: ";
  let message = read_line () in
  let coded_message =
    String.fold_left
      (fun acc letter -> acc @ (CharMap.find letter code))
      []
      message
  in
  Printf.printf "Le code du message est: %s\n" 
    (String.concat " " (List.map string_of_int coded_message));

  print_endline "Entrez un message codé à décoder (les entiers doivent être séparés avec des espaces): ";
  let coded_message = read_line () 
      |> String.split_on_char ' ' 
      |> List.map int_of_string in

  let decoded_message = decode_message coded_message code in
  Printf.printf "Le message décodé est: %s\n" (String.concat "" 
          (List.map (String.make 1) decoded_message))
;;
