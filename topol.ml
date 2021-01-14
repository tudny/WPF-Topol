
(* ******************************* *)
(* Topol (Sortowanie topologiczne) *)
(* Autor: Aleksander Tudruj        *)
(* Code review:                    *)
(* ******************************* *)

(* ***************** *)
(* Szkic rozwiązania *)
(* ***************** *)

(* Dla każadego wierzchołka zliczam ile krawędzi do niego wchodzi   *)
(* Tę liczbę nazwę stopniem wejścia                                 *)
(* Jeżeli dla danego wierzchołka stopień wejścia wynosi 0, to mogę  *)
(* dodać go do listy wynikowej oraz zmniejszyć stopień wejścia jego *)
(* dzieci, co rozumieć możemy jako usunięcie go z grafu             *)
(* Jeżeli stopień wejścia jakiegoś dziecka spadł do 0 to dodaję je  *)
(* do kolejki rozpatrywanych wierzchołków                           *)
(* Można zaoberwować, że jeżeli w grafie wystąpi cykl,              *)
(* to wierzchołki w tym cyklu nie zostaną nigdy dodane do kolejki   *)
(* skuktuje to niedodaniem ich do listy wynikowej                   *)
(* Zatem nie muszę robić testu na obecność cyklu w grafie;          *)
(* wystarczy sprawdzić czy lista wynikowa ma tyle wierzchołków      *)
(* ile jest ich w grafie                                            *)
(* Nie muszę również sprawdzać spójności grafu, gdyż nie wybieram   *)
(* Żadnego wierzchołka startowego, lecz konsekwentnie przechodzę    *)
(* się na początku po każdnym z nich                                *)

exception Cykliczne
;;

(* Zbieram wszystkie wierzchołki i tworzę mapy, na podstawie których  *)
(* mogę ponumerować wierzchołki. Później będę mógł zamienić numerację *)
(* na etykietki.                                                      *)
let collect_vertices_map data = 
  let (nodes, edges) = List.split data
  in
  let vertices = nodes @ List.flatten edges
  in
  let counter = ref 0
  in
  let increase () = 
    counter := !counter + 1;
    !counter - 1
  in
  let process (map_in, map_out) ele = 
    if PMap.mem ele map_in then (map_in, map_out)
    else 
      let id = increase () in
      (PMap.add ele id map_in, PMap.add id ele map_out) 
  in
  let (map_in, map_out) =
    List.fold_left (process) (PMap.empty, PMap.empty) vertices
  in
  (map_in, map_out, !counter)
;;

(* Typ wierzchołka w grafie             *)
(* label - etykietka, egdes - krawędzie *)
type node = { label : int; mutable edges : int list }
;;

(* Tworzę graf wierzchołków przenumerowanych na podstawie danych *)
(* wejściowych.                                                  *)
let create_graph data map_in counter = 
  let graph = Array.init counter (fun i -> { label = i; edges = [] })
  in
  List.iter
  ( fun (n, edges) ->
    let n_mapped = PMap.find n map_in in 
    List.iter
    ( fun e ->
      let e_mapped = PMap.find e map_in in 
      graph.(n_mapped).edges <- e_mapped :: graph.(n_mapped).edges
    ) edges
  ) data;
  graph
;;

(* Otrzymany wynik z powrotem zamieniam na oryginalne etykietki *)
let decode_result map_out result = 
  List.map (fun ele -> PMap.find ele map_out) result
;;


(*         Algorytm Sortowania Topologicznego         *)
(* Szkic działania w komentarzu na górze [@link 12:4] *)
let toposort graph counter = 
  let in_degree = Array.make counter 0
  in

  Array.iter
    ( fun { edges = edges } ->
      List.iter
        ( fun ele ->
          in_degree.(ele) <- in_degree.(ele) + 1
        ) edges
    ) graph;

  let queue = Queue.create ()
  in

  let add_to_queue_if_zero node = 
    if in_degree.(node) = 0 then Queue.add node queue
  in

  Array.iter
    ( fun { label = label } ->
      add_to_queue_if_zero label
    ) graph;
  
  let result = ref []
  in
  let add_result x = 
    result := x :: !result
  in

  while not (Queue.is_empty queue) do
    let front = Queue.take queue
    in
    add_result front;
    List.iter
      ( fun ele ->
        in_degree.(ele) <- in_degree.(ele) - 1;
        add_to_queue_if_zero ele
      ) graph.(front).edges
  done;

  if List.length !result <> counter then raise Cykliczne;
  !result |> List.rev
;;

(* Funkcja najpierw mapuje wartości, potem tworzy graf, *)
(* wywołuje algorytm sortowania topologicznego i zwraca *)
(* zdekodowany wynik.                                   *)
let topol data = 
  let map_in, map_out, counter = collect_vertices_map data
  in
  let graph = create_graph data map_in counter
  in
  let topo_sorted = toposort graph counter
  in
  decode_result map_out topo_sorted
;;


