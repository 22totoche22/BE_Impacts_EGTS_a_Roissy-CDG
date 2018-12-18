module Map = Lfpg_map
module Del = Delaunay

(* return la distance entre 2 points en géometrie 3d *)
let distance3D = fun point1 point2 ->
  let diffX = float (point1.Map.x - point2.Map.x) in
  let diffY = float (point1.Map.y - point2.Map.y) in
  let diffZ = point1.Map.z -. point2.Map.z in
  let distance = sqrt (diffX *. diffX +. diffY *. diffY +. diffZ *. diffZ) in
  distance;;

let distance2D = fun point1 point2 ->
  let diffX = float (point1.Map.x - point2.Map.x) in
  let diffY = float (point1.Map.y - point2.Map.y) in
  let distance = sqrt (diffX *. diffX +. diffY *. diffY) in
  distance;;

let vitesse_5s_3d = fun point1 point2 ->
  let distance = distance3D point1 point2 in
  let vitesse = distance /. 5. in
  vitesse;;

let altitudePoint = fun point triangle ->
  let a,b,c = triangle.Del.equa in
  let z = a *. (float point.Map.x) +. b *. (float point.Map.y) +. c in
  z;;
