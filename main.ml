
open Topol;;

let g = [(1, [2; 3]); (2, [4]); (3, [4; 5]); (4, []); (5, [])];;

let t = Topol.topol g;;

assert (t = [1; 2; 3; 4; 5]);;

let g2 = [(1, [2]); (2, [1])];;

try topol g2 with _ -> [];;

