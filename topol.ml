
exception Cykliczne

let map graph =
  List.fold_left
    (fun (_acc, cnt) (label, neighbours) ->
       (PMap.add label cnt _acc, cnt + 1)
    ) (PMap.empty, 0) graph
;;

let convert graph mapped =
  List.map (fun (label, neighbours) ->
      (PMap.find label mapped,
       List.map (fun neighbour -> PMap.find neighbour mapped) neighbours)
    ) graph
;;

let fill_in_degree graph in_degree =
  List.iter (fun (label, neighbours) ->
      List.iter (fun neighbour ->
          in_degree.(neighbour) <- in_degree.(neighbour) + 1
        ) neighbours
    ) graph
;;

let fill_queue queue in_degree =
  Array.iteri
    (fun i degree -> if degree = 0 then Queue.add i queue) in_degree
;;

let dec_degree neighbours in_degree =
  List.iter
    (fun neighbour -> in_degree.(neighbour) <- in_degree.(neighbour) - 1)
    neighbours
;;

let make_up_queue neighbours in_degree queue =
  List.iter
    (fun neighbour ->
       if in_degree.(neighbour) = 0 then Queue.add neighbour queue)
    neighbours
;;

let topol graph =
  let (mapped, nr_of_nodes) = map graph in
  let converted = convert graph mapped in
  let arr_graph = Array.of_list converted in
  let in_degree = Array.make nr_of_nodes 0 in
  fill_in_degree converted in_degree;
  let queue = Queue.create () in
  fill_queue queue in_degree;
  let score = ref [] in
  let add_to_score x = score := x :: !score in
  while not (Queue.is_empty queue) do
    let element = Queue.take queue in
    add_to_score element;
    dec_degree (arr_graph.(element) |> snd) in_degree;
    make_up_queue (arr_graph.(element) |> snd) in_degree queue
  done;
  let reversed_map =
    PMap.foldi
      (fun key value _acc -> PMap.add value key _acc)
      mapped PMap.empty in
  if List.length !score <> nr_of_nodes then raise Cykliczne
  else
    List.fold_left (fun _acc el -> PMap.find el reversed_map :: _acc) [] !score
;;




