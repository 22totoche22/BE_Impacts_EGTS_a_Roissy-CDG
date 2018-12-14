module Map = Lfpg_map
module Visu = Visu
module Del = Delaunay
  
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

(*renvoie les routes des avions *)
let new_routes new_flights =
  List.map (fun i -> i.Map.route ) new_flights ;;

(* renvoie la liste des avions de la simulation en enlevant les avions  ayant finis *)
let enleve_avion listeavion_simulation time =
  List.filter (fun i -> i.Map.h_arr <> time ) listeavion_simulation ;;

(* route sans le premier point *)
let  new_trajectory route =
  let point = hd route in 
  let new_trajectoire = tl route in
  (point,new_trajectoire);;

(* renvoie les trajectoires sans le premier point *)
let new_trajectories trajectories =
  List.map (fun route -> let (pt,traj) = new_trajectory route in traj) trajectories;;

(* renvoie les premiers points des trajectoires *)
let new_points trajectories =
  List.map (fun route -> let (pt,traj) = new_trajectory route in pt) trajectories;;
    

(* simulation *)
let simulation (marks,runways,taxiways,listetriangle,listeavion) dt =
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
	let newplanes = new_flights listeavion time in
	let newroutes = new_routes newplanes in
	let trajectoires = List.append newroutes trajectoires in (* on ajoute les nouvelles trajectoires *)
	let points = new_points trajectoires in
	let new_trajectoires = new_trajectories trajectoires in
	let new_avion_simulation = List.append newplanes listeavion_simulation in (* on ajoute les nouveaux avions *)
        let new2_listeavion_simulation = enleve_avion new_avion_simulation time in 
	Visu.move_flights points largeur largeur_max hauteur hauteur_max time;
	let new_time = time + dt in
	simu new_time new2_listeavion_simulation new_trajectoires end in
  simu debut_time  debut_listeavion_simulation debut_trajectoires;
  with Graphics.Graphic_failure _ -> print_endline "Exiting..." ;;


let () =
    simulation (Map.marks,Map.runways,Map.taxiways,Del.listeTriangle,Map.flights) 5;;
 
