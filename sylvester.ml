(* 
  Si vous voulez construire des grosses matrices pour les visualizer il vaut mieux 
  rediriger la sortie d'erreur standard vers un fichier comme ceci:
    ./sylvester 2> mon_fichier 
*)
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
  print_endline "Programme de construction des matrices Hadamard avec la construction de sylvester";
  print_string "Donnez l'ordre de la matrice que vous voulez construire: ";
  let order = read_int () in
  let hadamard_matrix = sylvester order in
  print_matrix hadamard_matrix;
