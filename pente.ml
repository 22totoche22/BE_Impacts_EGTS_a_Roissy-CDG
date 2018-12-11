module Map = Lfpg_map
module Del = Delaunay

(* return la distance entre 2 points en géometrie 3d *)
let distance3D = fun point1 point2 ->
  let diffX = float (point1.Map.x - point2.Map.x) in
  let diffY = float (point1.Map.y - point2.Map.y) in
  let diffZ = point1.Map.z -.point2.Map.z in
  let distance = sqrt (diffX *. diffX +. diffY *. diffY +. diffZ *. diffZ) in
  distance;;

