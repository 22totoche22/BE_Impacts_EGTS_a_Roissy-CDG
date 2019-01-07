
module Map = Lfpg_map
module Accel = Accel
module Geo = GeometrieTriangle

type avion_conflit = Map.point array array (* fenetre des avions déjà résolus (trajectoireavion/temps *) (* faire une sliding window *)

(* transforme la liste des points de la trajectoire en tableau *)
let trajectoire avion =
  Array.of_list avion.Map.route;; 
  
(* renvoie la position de l'avion au temps t*)
let position_temps tab_trajectoires temps =
  tab_trajectoires.(temps/5);;

(* renvoie true si le point A et le point B sont trop proches *)
let rencontre pointA pointB =
  let rayon = 50. in  (* nos avions ont des longeurs de 32m *)
  let dis_ab = float (pointA.Map.x - pointB.Map.x ) ** 2. +. float (pointA.Map.y - pointB.Map.y ) ** 2. in
  (sqrt ( dis_ab)) < (2. *. rayon) ;; (* |R-R'| < AB < R+R' *)

(* renvoie true si l'avion est en conflit au temps t avec les autres avions déjà resolus *)
let avion_conflit fenetre ligne position_avion t_fenetre =
  let conflit = ref false in
  let rec rec_conflit i =
    if !conflit || i >= (ligne-1) then ()
    else
      begin
	conflit := rencontre fenetre.(i).(t_fenetre) position_avion;
	rec_conflit (i + 1) ;
      end in
  rec_conflit 0;
  !conflit;;

let w08L = {Map.x = 363; y = -1583; z = 0.} in
let x08L = {Map.x = 363; y = -1543; z = 0.} in
let y08L = {Map.x = 4566; y = -1227; z = 0.} in
let z08L = {Map.x = 4566; y = -1187; z = 0.} in
let runway08L = {Geo.p1 = w08L; p2 = x08L; p3 = y08L; p4 = z08L}in ();;

let w09R = {Map.x = -2545; y = 1221; z = 0.} in
let x09R = {Map.x = -2545; y = 1181; z = 0.} in
let y09R = {Map.x = 1639; y = 1575; z = 0.} in
let z09R = {Map.x = 1639; y = 1535; z = 0.} in
let runway09R = {Geo.p1 = w09R; p2 = x09R; p3 = y09R; p4 = z09R}in ();;

(* rajoute la trajectoire d'un avion sans conflit *)
let ajout_avion_resolu avion fenetre ligne dt triangulation =
  let tab_trajectoire = trajectoire avion in
  let length = Array.length tab_trajectoire in
  let time_debut = avion.Map.h_dep in
  let debut = ref tab_trajectoire.(0) in
  let fin = tab_trajectoire.(length -1) in
  let vitesseAvant = ref 0. in
  let compteurTempsA5s = ref 0. in
  let longueur_liste = ref 0 in
  let rec solve_conflit position temps_t =


    (* on met la position dans le tableau si pas de conflit sinon on passe la vitesse de l'avion à 0 *)
    match not (avion_conflit !fenetre  ligne !position ((time_debut + temps_t)/5)) with
      true ->
    
	Array.set (!fenetre).(ligne) ((time_debut + temps_t)/5) !position ;
	  
      (* calcul de la nouvelle position *)
	let traject = ref [] in
	let next_position = ref (position_temps tab_trajectoire (temps_t+dt)) in
	traject := Accel._calculTrajectoireEntre2points (position_temps tab_trajectoire (temps_t)) !position !next_position avion.Map.flight_category avion.Map.masse triangulation compteurTempsA5s (float dt) vitesseAvant avion.Map.flight_stand;
	longueur_liste := List.length !traject;

	if !longueur_liste = 0
	then
	  begin
	    next_position := position_temps tab_trajectoire (temps_t + 2 * dt);
	    traject := Accel._calculTrajectoireEntre2points (position_temps tab_trajectoire (temps_t + dt))  !position !next_position avion.Map.flight_category avion.Map.masse triangulation compteurTempsA5s (float dt) vitesseAvant avion.Map.flight_stand;
	  end;	
	let new_position = ref (List.hd !traject) in
	Printf.printf "pos %d %d \n" (!position).Map.x (!position).Map.y;
	Printf.printf "pos'%d %d \n" (!next_position).Map.x (!next_position).Map.y;
	
	
      (* si c'est la fin ou en continuant ou en s'arretant *)
	(position_temps tab_trajectoire (temps_t+dt)) = fin  || solve_conflit new_position (temps_t + dt) || solve_conflit position (temps_t +dt)
	    
    |false -> vitesseAvant := 0.;false
  in
  solve_conflit debut 0 ;;




