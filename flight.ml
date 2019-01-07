module Map = Lfpg_map
module Visu = Visu
module Del = Delaunay
module Accel = Accel
module Backtrack = Backtrack2
  
let print_avion_simulation listeavion_simulation =
  List.iter (fun avion -> print_string avion.Map.flight_id; print_string " : " ) listeavion_simulation ;;

(* soucis liste vide à régler *)
let hd liste =
  let head = ref {Map.x = 1000000; Map.y = 100000; Map.z = 0.} in
  begin
    match liste with 
    [] -> ()
  | a::l -> head:= a;
  end;
  !head;;

let tl liste = 
  let tail = ref [] in
  begin
    match liste with
      [] -> ()
    | a::l -> tail:= l ;
  end;
  !tail;;



(* renvoie la liste des nouveaux avions à intégrer dans la simulation  *)
let new_flights listeavion time  =
  List.filter (fun i -> i.Map.h_dep = time ) listeavion ;;

(*renvoie les routes des avions signées*)
let new_routes new_flights =
  List.map (fun i -> (i.Map.route,i.Map.flight_stand)) new_flights ;;

(* renvoie la liste des avions de la simulation en enlevant les avions  ayant finis *)
let enleve_avion listeavion_simulation time =
  List.filter (fun i -> i.Map.h_arr <> time ) listeavion_simulation ;;

(* route sans le premier point *)
let  new_trajectory route =
  let point = hd route in 
  let new_trajectoire = tl route in
  (point,new_trajectoire);; (* (point signe, liste de points renvoie) *)

(* signés les trajectoires sans le premier point *)
let new_trajectories trajectories =
  List.map (fun (route,s) ->  let (pt,traj) = new_trajectory route in (traj,s))  trajectories;;

(* renvoie les premiers points des trajectoires *)
let new_points trajectories =
  List.map (fun (route,s) ->  let (pt,traj) = new_trajectory route in (pt,s)) trajectories;;
    
(*
(* simulation *)
let simulation (marks,runways,taxiways,listetriangle,listeavion) dt vitesse=
  Graphics.open_graph("");
  let (largeur, hauteur) = (1200,800) in
  Graphics.resize_window largeur hauteur;
  let (largeur_max,hauteur_max) = (10000,8000) in
  try Visu.draw_airport (marks,runways,taxiways,listetriangle) largeur largeur_max hauteur hauteur_max;
  
  let debut_time = 0 in
  let  debut_listeavion_simulation = [] in
  let debut_trajectoires = [] in 
  let rec simu time listeavion_simulation trajectoires =
    if time = 86400 (* 24h en seconde *)
    then print_string "terminé"
    else
      begin
	let newplanes = new_flights listeavion time in (* liste des avions à intégrer dans la simu *)
	let newroutes = new_routes newplanes in (* liste de (route de l'avion,signature)  *)
	let trajectoires = List.append newroutes trajectoires in (* on ajoute les nouvelles routes *)
	let points = new_points trajectoires in (* extraction liste des points signés à l'instant t  *)
	let new_trajectoires = new_trajectories trajectoires in 
	let new_avion_simulation = List.append newplanes listeavion_simulation in (* on ajoute les nouveaux avions *)
        let new2_listeavion_simulation = enleve_avion new_avion_simulation time in 
	Visu.move_flights points largeur largeur_max hauteur hauteur_max time vitesse;
	let new_time = time + dt in
	simu new_time new2_listeavion_simulation new_trajectoires end in
  simu debut_time  debut_listeavion_simulation debut_trajectoires;
  with Graphics.Graphic_failure _ -> print_endline "Exiting..." ;;
*)
exception Trajectory
(*
let rec trajectoires_altitude quantite liste_avion liste_avion_tire triangulation timeSimulation =
  if quantite = 0 then
    liste_avion
  else
    begin
      let size = List.length liste_avion in
      let rec nombre_tire () =
	let n = Random.int size in
	if not (List.mem n liste_avion_tire) then n
	else  nombre_tire () in
      let nombre = nombre_tire() in
      let new_liste_avion_tire = nombre::liste_avion_tire in
      let liste_avion_tire = new_liste_avion_tire in
      let plane = List.nth liste_avion nombre in
      let avion = ref Accel.a319 in
      let masse = ref 0. in
      begin
	match plane.Map.flight_category with
	  cat when cat = "L" ->  avion:= Accel.a319
	|cat when cat = "M" ->  avion:= Accel.a320
	|cat when cat = "H" ->  avion:= Accel.a321
	|_ -> failwith "category" ;
      end;
      begin
	match plane.Map.dep_arr with
	  dep_ar when dep_ar = "DEP" -> masse := (!avion).Accel.mass_dep 
	|dep_ar when dep_ar = "ARR" -> masse  := (!avion).Accel.mass_arri 
	|_ -> failwith "depart_arrive" ;
      end;
      plane.Map.flight_stand <- "E";
      let new_traject = Accel.calculTrajectoireTotal  plane.Map.route !avion !masse triangulation timeSimulation plane.Map.flight_stand in
      plane.Map.route <- new_traject ;
      trajectoires_altitude (quantite - 1) liste_avion liste_avion_tire triangulation timeSimulation;
end;;
*)

let simulation (marks,runways,taxiways,listetriangle,listeavion) triangulation fenetre dt vitesse=
  Graphics.open_graph("");
  let (largeur, hauteur) = (1200,800) in
  Graphics.resize_window largeur hauteur;
  let (largeur_max,hauteur_max) = (10000,8000) in
  try Visu.draw_airport (marks,runways,taxiways,listetriangle) largeur largeur_max hauteur hauteur_max;
      
      let listeaviontrie =  List.sort (fun a b  -> if a.Map.h_dep < b.Map.h_arr then -1 else 1) listeavion in
      List.iteri (fun i avion -> begin print_string "avion : " ;print_int i ; let _ = Backtrack.ajout_avion_resolu avion fenetre i dt triangulation in () end ) listeaviontrie;
      let debut_time = 0 in
      print_string "solve";
      let rec simu time =
	if time = 86400 (* 24h en seconde *)
	then print_string "terminé"
	else
	  begin
	    let points = ref [] in
	    let pointnul = {Map.x = 0; y = 0; z = 0.} in
	    Array.iteri (fun nb i ->
	      let _avion = List.nth listeaviontrie nb in
	      if not(i.(time/5).Map.x = pointnul.Map.x && i.(time/5).Map.y = pointnul.Map.y)
	      then points := (i.(time/5),_avion.Map.flight_stand )::!points
	    )  !fenetre;
	    
	    (* List.iter (fun (i,b) -> Printf.printf "%d %d " i.Map.x i.Map.y) !points;
	       Printf.printf "\n nouveau dt \n"; *)
	    Visu.move_flights !points largeur largeur_max hauteur hauteur_max time vitesse;
	    let new_time = time + dt in
	    simu new_time 
	  end in
      simu debut_time;
  with Graphics.Graphic_failure _ -> print_endline "Exiting..." ;;


  
let rec trajectoires_altitude quantite liste_avion liste_avion_tire   =
  if quantite = 0 then
    liste_avion
  else
    begin
      let size = List.length liste_avion in
      let rec nombre_tire () =
	let n = Random.int size in
	if not (List.mem n liste_avion_tire) then n
	else  nombre_tire () in
      let nombre = nombre_tire() in
      let new_liste_avion_tire = nombre::liste_avion_tire in
      let liste_avion_tire = new_liste_avion_tire in
      let plane = List.nth liste_avion nombre in
      plane.Map.flight_stand <- "E";
      trajectoires_altitude (quantite - 1) liste_avion liste_avion_tire;
    end;;
  



let () =
  let fenetre = ref (Array.make_matrix 1573 20000 {Map.x = 0; y =0 ; z = 0.}) in
  let _ = trajectoires_altitude 300 Map.flights [] in
  simulation (Map.marks,Map.runways,Map.taxiways,Del.listeTriangle,Map.flights) Del.listeTriangle fenetre 5 20.;;



