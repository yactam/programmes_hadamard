let kronecker_product a b =
  let rows_a, cols_a = Array.length a, Array.length a.(0) in
  let rows_b, cols_b = Array.length b, Array.length b.(0) in
  let result_rows, result_cols = rows_a * rows_b, cols_a * cols_b in
  let result = Array.make_matrix result_rows result_cols 0 in

  for i = 0 to rows_a - 1 do
    for j = 0 to cols_a - 1 do
      for k = 0 to rows_b - 1 do
        for l = 0 to cols_b - 1 do
          result.(i * rows_b + k).(j * cols_b + l) <- a.(i).(j) * b.(k).(l)
        done;
      done;
    done;
  done;

  result

let hadamard_kronecker k =
  if k <= 0 then [||]
  else
    let h2 = [|[|1; 1|]; [|1; -1|]|] in
    let rec loop i acc =
      if i = k then acc
      else
        let h2k_minus_1 = loop (i + 1) acc in
        kronecker_product h2k_minus_1 h2
    in
    loop 1 h2

let print_matrix (matrix : int array array) : unit =
  Array.iter (fun row -> 
    Array.iter (fun elem ->
      if elem = -1 
      then Printf.fprintf stderr "  "
      else Printf.fprintf stderr "██";) 
    row; Printf.fprintf stderr "\n") matrix;;

let rec read_int () =
  try 
    let n = int_of_string (read_line ()) 
    in if (n <= 0) then (print_string "Veuillez saisir un entier strictement positif: ";
    read_int ()) else n 
  with
  | Failure _ -> print_string "Veillez saisir un entier valid: "; read_int ()

let () =
  print_endline "Programme de construction des matrices Hadamard avec le produit de Kronecker";
  print_string "Donnez l'ordre de la matrice que vous voulez construire: ";
  let order = read_int () in
  let hadamard_matrix = hadamard_kronecker order in
  print_matrix hadamard_matrix;

