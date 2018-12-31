
module Map = Lfpg_map
module Accel = Accel 


type avion_conflit = Map.point array array (* fenetre des avions déjà résolus (trajectoireavion/temps *) (* faire une sliding window *)

(* transforme la liste des points de la trajectoire en tableau *)
let trajectoire avion =
  Array.of_list avion.Map.route;; 
  
(* renvoie la position de l'avion au temps t*)
let position_temps tab_trajectoires temps =
  tab_trajectoires.(temps/5);;

(* renvoie true si le point A et le point B sont trop proches *)
let rencontre pointA pointB =
  let rayon = 40. in  (* nos avions ont des longeurs de 32m *)
  let dis_ab = float (pointA.Map.x - pointB.Map.x ) ** 2. +. float (pointA.Map.y - pointB.Map.y ) ** 2. in
  (sqrt ( dis_ab)) < (2. *. rayon) ;; (* |R-R'| < AB < R+R' *)

(* renvoie true si l'avion est en conflit au temps t avec les autres avions déjà resolus *)
let avion_conflit fenetre trajectoire_avion t_avion t_fenetre =
  let conflit = ref false in
  let taille = Array.length trajectoire_avion in
  let rec rec_conflit i =
    if !conflit || i = taille then ()
    else
      begin
	conflit := rencontre fenetre.(i).(t_fenetre) trajectoire_avion.(t_avion);
	rec_conflit (i + 1) ;
      end in
  rec_conflit 0;
  !conflit;;

(*
(* rajoute la trajectoire d'un avion sans conflit *)
let ajout_avion_resolu avion fenetre ligne dt triangulation =
  let tab_trajectoire = trajectoire avion in
  let length = Array.length tab_trajectoire in
  let time_debut = avion.Map.h_dep in
  let debut = tab_trajectoire.(0) in
  let fin = tab.trajectoire.(length -1) in
  let vitesse = ref 0. in
  let rec solve_conflit position temps_t vitesse =
    let position = position_temps tab_trajectoire temps_t in
    if not (avion_conflit fenetre trajectoire_avion temps_t (time_debut + temps_t))
    then Array.set fenetre.(ligne) temps_t position 
    else  vitesse:= 0.;
    let new_position = List.hd (
      Accel.calculTrajectoireEntre2points  position next_position avion.Map.flight_category avion.Map.masse triangulation compteurTempsA5s timeSimulation vitesseAvant avion.Map.flight_stand) in
    new_position = fin  || solve_conflit new_position (temps_t + dt) !vitesse ||  solve_conflit position (temps_t +dt) !vitesse in
  solve_conflit debut 0 !vitesse;;
    
*)



