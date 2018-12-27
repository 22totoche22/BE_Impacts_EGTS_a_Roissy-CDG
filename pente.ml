module Map = Lfpg_map
module Del = Delaunay

(* retourne la distance entre 2 points en géometrie 3d *)
let distance3D = fun point1 point2 ->
  let diffX = float (point1.Map.x - point2.Map.x) in
  let diffY = float (point1.Map.y - point2.Map.y) in
  let diffZ = point1.Map.z -. point2.Map.z in
  let distance = sqrt (diffX *. diffX +. diffY *. diffY +. diffZ *. diffZ) in
  distance;;

(* retourne la distance entre 2 points en géometrie 2d *)
let distance2D = fun point1 point2 ->
  let diffX = float (point1.Map.x - point2.Map.x) in
  let diffY = float (point1.Map.y - point2.Map.y) in
  let distance = sqrt (diffX *. diffX +. diffY *. diffY) in
  distance;;

(* retourne la vitesse pour un temps de 5s et une distance connue entre 2 points *)
let vitesse_5s_3d = fun point1 point2 ->
  let distance = distance3D point1 point2 in
  let vitesse = distance /. 5. in
  vitesse;;

(* retourne l'altitude d'un point a partir de l'equation du triangle auquel il appartient *)
let altitudePoint = fun point triangle ->
  let a,b,c = triangle.Del.equa in
  let z = a *. (float point.Map.x) +. b *. (float point.Map.y) +. c in
  z;;
