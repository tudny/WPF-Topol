
(* open Topol;; *)

let is_correct input output = 
  let toposorted_arr = Array.of_list output in

  let find arr x = 
    let id = ref (-1) in 
    Array.iteri
      (fun i el -> if el = x then id := i)
      arr; !id
  in let find = find toposorted_arr in

  let greater x y = find x < find y in
  List.map
    (fun (node, children) ->
      List.for_all (greater node) children
    ) input |> List.for_all (fun x -> x) |> not
;;


let nr_of_tests = 10;;

let max_nr = 100;;

let generate seed = 
  Random.init seed;
  let n = Random.int max_nr in
  let graph = Array.init n (fun id -> (id, [])) in
  let _gen m = List.init m (fun i -> Random.int n) in
  for i = 0 to n - 1 do
    graph.(i) <- (i, _gen (Random.int (n / 2)))
  done;
  Array.to_list graph
;;

let graph = generate 21;;
let topsort = Topol.topol graph;;

assert (is_correct graph topsort);;

(* [(0, [1]); (1, [7]); (2, [1]); (3, [7]); (4, [9; 8; 1]); (5, [10]);
   (6, [3; 0; 10]); (7, [4]); (8, [2; 7; 2]); (9, [5]);
   (10, [])] *)
let g = [(0, [1]); (1, [7]); (2, [1]); (3, [7]); (4, [9; 8; 1]); (5, [10]);
   (6, [3; 0; 10]); (7, [4]); (8, [2; 7; 2]); (9, [5]);
   (10, [])];;

Topol.topol g;;

