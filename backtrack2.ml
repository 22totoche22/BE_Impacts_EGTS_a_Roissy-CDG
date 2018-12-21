
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
let avion_conflit fenetre trajectoire_avion t =
  let conflit = ref false in
  let taille = Array.length trajectoire_avion in
  let rec rec_conflit i =
    if !conflit || i = taille then ()
    else
      begin
	conflit := rencontre fenetre.(i).(t) trajectoire_avion.(t);
	rec_conflit (i + 1) ;
      end in
  rec_conflit 0;
  !conflit;;
(*A MODIFIER  pour les avions, leur time propre est time + heduredebut *) 
(*
(* rajoute la trajectoire d'un avion sans conflit *)
let ajout_avion_resolu avion =
  let tab_trajectoire = trajectoire avion in
  let time_debut = avion.Map.h_dep in
  let rec solve_conflit position avion temps_t vitesse =
    let position = position_temps tab_trajectoires temps_t 
      avion_conflit fenetre trajectoire_avion


      Sans_conflit position temps
      next_position = 

    append
  
    next_position = fin 
  || solve_conflit next_position next_temps_t next_vitesse
  || solve_conflit position next_temps_t 0


*)


