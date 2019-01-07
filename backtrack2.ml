
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
  let rayon = 100. in  (* nos avions ont des longeurs de 32m *)
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



(* rajoute la trajectoire d'un avion sans conflit *)
let ajout_avion_resolu avion fenetre ligne dt triangulation =
  let tab_trajectoire = trajectoire avion in
  let length = Array.length tab_trajectoire in
  let time_debut = avion.Map.h_dep in
  let debut = tab_trajectoire.(0) in
  let fin = tab_trajectoire.(length -1) in
  let vitesseAvant = ref 0. in
  let compteurTempsA5s = ref 0. in
  
  let rec solve_conflit position temps_t vitesse =
    let position = position_temps tab_trajectoire temps_t in
    let new_position = ref position in
    let next_position = ref position in
    
    if not ((!next_position).Map.x = fin.Map.x && (!next_position).Map.y = fin.Map.y )
    then  next_position := (position_temps tab_trajectoire (temps_t + dt));
    
    (* on met la position dans le tableau si pas de conflit sinon on passe la vitesse de l'avion à 0 *)
    if not (avion_conflit !fenetre  ligne position ((time_debut + temps_t)/5))
    then  Array.set (!fenetre).(ligne) ((time_debut + temps_t)/5) position 
    else  vitesseAvant:= 0.;
    
    (* disjonction de cas si compteurTempsà5s < 5s *)
    if (!compteurTempsA5s <  float dt) && not((!next_position).Map.x = fin.Map.x && (!next_position).Map.y = fin.Map.y )
    then
      begin
	next_position := position_temps tab_trajectoire (temps_t + 2 * dt) ;
      end;
    let trajec = Accel.calculTrajectoireEntre2points  position !next_position avion.Map.flight_category avion.Map.masse triangulation compteurTempsA5s (float dt) vitesseAvant avion.Map.flight_stand in
    if trajec = [] then () 
    else new_position := List.hd trajec ;
    !new_position = fin  || solve_conflit !new_position (temps_t + dt) vitesse in
  solve_conflit debut 0 !vitesseAvant;;



(*
let ajout_avion_resolu avion fenetre ligne dt triangulation =
  let list_trajectoire = ref avion.Map.route in
  let time_debut = avion.Map.h_dep in
  let rec solve_conflit temps_t trajectoire =
    
    let position = ref {Map.x = 0; y =0; z=0.} in
    begin
	  match !trajectoire with
	  hd::_ -> position := hd; 
	  |_ -> failwith "liste vide1";
    end;

    (* on met la position dans le tableau si pas de conflit sinon on recalcule la trajectoire et on la met à jour *)
    if not (avion_conflit !fenetre  ligne !position ((time_debut + temps_t)/5))
    then
      begin
	Array.set (!fenetre).(ligne) ((time_debut + temps_t)/5) !position ;
	begin
	  match !trajectoire with
	  hd::tail -> trajectoire := tail
	  |_ -> failwith "liste vide2";
	end
      end
    else
      begin
      trajectoire := Accel.calculTrajectoireTotal !trajectoire avion.Map.flight_category avion.Map.masse triangulation (float dt) avion.Map.flight_stand;
      end;
    !trajectoire = []  || solve_conflit (temps_t + dt) trajectoire in
  
  solve_conflit 0 list_trajectoire;;
*)
