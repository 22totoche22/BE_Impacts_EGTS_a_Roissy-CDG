module Map = Lfpg_map
module Del = Delaunay
module Pivot = Pivot
module Geo = GeometrieTriangle
  
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

(* calcul min d'une liste de float *)
let mini = fun liste ->
  let minimum = ref (infinity) in
  let rec loop = fun listep ->
    match listep with
	[] -> ()
      | z::reste ->
	if z < !minimum then minimum := z;
	loop reste;
  in loop liste;
  !minimum;;

(* calcul max d'une liste de float *)
let maxi = fun liste ->
  let maximum = ref (neg_infinity) in
  let rec loop = fun listep ->
    match listep with
	[] -> ()
      | z::reste ->
	if z > !maximum then maximum := z;
	loop reste;
  in loop liste;
  !maximum;;


(* calcule le point le plus haut et le plus bas de l'aeroport *)
let calculAltiMinMax = fun listePointAeroport delaunay ->
  let listeAlti = ref [] in
  let rec loop = fun listePoint ->
    match listePoint with
	[] -> ()
      | point::reste ->
	let listeTriangle = Geo.dansQuelTriangle point delaunay in
	begin
	  match listeTriangle with
	      [] -> ()
	    | triangle::queue ->
	      let z = altitudePoint point triangle in
	      listeAlti := z::(!listeAlti);
	end;
	loop reste;
  in loop listePointAeroport;
  let zmin = mini !listeAlti in
  let zmax = maxi !listeAlti in
  zmin,zmax;;

(* calcul de l'altitude Min et Max sur l'aeroport *)
(* ATTENTION liste de point de départ à vérifier *)
let listePoint = Map.point_xyz_points Map.points_alti
let delau = Pivot.triangle_equa 
let zmin,zmax = calculAltiMinMax listePoint delau
let () = Printf.printf "min : %f max : %f denivele : %f\n" zmin zmax (zmax -. zmin)
