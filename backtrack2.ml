
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
  let rayon = 5. in  (* nos avions ont des longeurs de 32m *)
  let dis_ab = float (pointA.Map.x - pointB.Map.x ) ** 2. +. float (pointA.Map.y - pointB.Map.y ) ** 2. in
  (sqrt ( dis_ab)) < (2. *. rayon) ;; (* |R-R'| < AB < R+R' *)

(* renvoie true si l'avion est en conflit au temps t avec les autres avions déjà resolus *)
let avion_conflit fenetre ligne position_avion t_fenetre =
  let conflit = ref false in
  let rec rec_conflit i =
    if !conflit || i >= (ligne-1) then ()
    else
      begin
	(* Printf.printf "fen %d %d \n" fenetre.(i).(t_fenetre).Map.x fenetre.(i).(t_fenetre).Map.y;
	   Printf.printf "pt %d %d \n" position_avion.Map.x position_avion.Map.y;*)
	conflit := rencontre fenetre.(i).(t_fenetre) position_avion;
	if !conflit then Printf.printf "conflit entre l'avion  %d et l'avion %d au temps %d pour la position %d %d et la position %d %d \n" i ligne t_fenetre position_avion.Map.x position_avion.Map.y fenetre.(i).(t_fenetre).Map.x fenetre.(i).(t_fenetre).Map.y;
	
	rec_conflit (i + 1) ;
      end in
  rec_conflit 0;
  !conflit;;


let sans_conflit fenetre ligne position temps =
  Printf.printf "%d %d\n" position.Map.x position.Map.y;
  Printf.printf "%d \n" temps;
  let sansconflit = not (avion_conflit !fenetre  ligne position temps) in
  
    if sansconflit
    then
      begin
	Array.set (!fenetre).(ligne) temps position;
	true;
      end
    else
      begin
	(* Printf.printf "non\n";*)
	false
      end;;

let next_traject trajectoire =
  let new_trajectoire = ref [] in
  begin
      match trajectoire with
	hd::tail -> new_trajectoire := tail;
      |_ -> failwith "liste vide2";
  end;
  !new_trajectoire;;

let next_position trajectoire =
  let next_pos = ref {Map.x = 0; y =0; z=0.} in
  begin
      match trajectoire with
	hd::_ -> next_pos := hd;
      |_ -> failwith "liste vide1";
  end;
  !next_pos;;

let trajectoire_arret trajectoire category masse triangulation temps stand =
  Printf.printf "on recalcule la trajectoire \n";
  let new_trajectoire = ref [] in
  new_trajectoire := Accel.calculTrajectoireTotal trajectoire category masse triangulation temps stand;
  !new_trajectoire;;

let ajout_avion_resolu avion fenetre ligne dt triangulation =
  let tab_trajectoire = trajectoire avion in
  let list_trajectoire = Accel.calculTrajectoireTotal avion.Map.route avion.Map.flight_category avion.Map.masse triangulation (float dt) avion.Map.flight_stand in
  let length = Array.length tab_trajectoire in
  let time_debut = avion.Map.h_dep in
  let fin = tab_trajectoire.(length -1) in
  let rec solve_conflit trajectoire temps_t  =
      (List.length trajectoire = 0 ||
	    (sans_conflit fenetre ligne (List.hd trajectoire) ((time_debut + temps_t)/5))) &&
	    (List.length trajectoire = 0 || next_position trajectoire = fin ||
		solve_conflit (next_traject trajectoire) (temps_t + dt) ||
		solve_conflit (trajectoire_arret trajectoire avion.Map.flight_category avion.Map.masse triangulation (float dt) avion.Map.flight_stand) (temps_t +dt))
  in
  solve_conflit list_trajectoire 0;;




(*
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
*)
(*
(* rajoute la trajectoire d'un avion sans conflit *)
let ajout_avion_resolu avion fenetre ligne dt triangulation =
  let tab_trajectoire = trajectoire avion in
  let list_trajectoire = ref avion.Map.route in
  let length = Array.length tab_trajectoire in
  let time_debut = avion.Map.h_dep in
  let fin = tab_trajectoire.(length -1) in
  let rec solve_conflit trajectoire temps_t  =

    let position = ref {Map.x = 0; y =0; z=0.} in
    begin
      match !trajectoire with
	hd::_ -> position := hd; 
      |_ -> failwith "liste vide1";
    end;
    (*Printf.printf "%d %d \n " (!position).Map.x (!position).Map.y;*)
    (* on met la position dans le tableau si pas de conflit sinon on passe la vitesse de l'avion à 0 *)
    match not (avion_conflit !fenetre  ligne !position ((time_debut + temps_t)/5)) with
      true ->
	let new_trajectoire = ref [] in
        Array.set (!fenetre).(ligne) ((time_debut + temps_t)/5) !position ;
	begin
	  match !trajectoire with
	  hd::tail -> new_trajectoire := tail
	  |_ -> failwith "liste vide2";
	end;
	
	let new_position = ref (List.hd !new_trajectoire) in
	(* si c'est la fin ou en continuant ou en s'arretant *)
	List.length !new_trajectoire = 1 || !new_position = fin || solve_conflit new_trajectoire (temps_t + dt) 
	    
    |false ->
       Printf.printf "la";
       trajectoire := Accel.calculTrajectoireTotal !trajectoire avion.Map.flight_category avion.Map.masse triangulation (float dt) avion.Map.flight_stand;
      let new_position = ref (List.hd !trajectoire) in
      List.length !trajectoire = 1 || !new_position = fin ||solve_conflit trajectoire (temps_t +dt)
      
  in
  solve_conflit list_trajectoire 0;;
*)
