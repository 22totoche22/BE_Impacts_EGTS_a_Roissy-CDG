module Pente = Pente
module Map = Lfpg_map
module Flight = Flight
module Accel = Accel

let listeVolsCourrants = fun listFlights currentTime ->
	List.filter (fun i -> i.Map.h_dep = currentTime ) listFlights ;;

(* fun detectionConflits : *)
(* tous les vols avancent de 5s 
   pour tous les vols i de listeVols faire
     pour tous les vols j de volI::resteListeVols faire
       si positionCourantevolI == positionCourantevolJ alors
         conf = true;
   listeConflits = listeConflits::volI; *)
  
let detectionConflits = fun conf listeVols listConf currentTime ->
  
  let rec loop listeAv =
    match listeAv with
	[] -> ()
      | vol::queue -> begin
	let indexTraj = (currentTime - vol.Map.h_dep) / 5 in
	
	let rec loopInt av liste =
	  match queue with
	      [] -> ()
	    | volCompare::reste -> begin
	      let indexTrajCompare = (currentTime - volCompare.Map.h_dep) / 5 in
	      if (Pente.distance3D (List.nth vol.Map.route indexTraj) (List.nth volCompare.Map.route indexTrajCompare)) < 5. then
			begin
		  		conf := true;
		  		listConf := vol::(!listConf); 
		  		(* problème : si le vol est en conflit avec plusieurs autres vols, alors il sera ajouté plusieurs fois à la liste*)
			end;
	    end;
	      loopInt vol reste in
	
	loopInt vol queue;
      end;
	loop queue in
  
  loop listeVols;
  ((listConf), (conf));;


(* pour tous les vols de listConf faire
     pour tous les vols i de Listvols otée de ListConf faire
       positionvolConflitT = positionvolConflitT-5s;
       si positionvolConflitT = positionvolIT alors
         recalcul de la trajetoire de volConflit
         retirer volConf de listConf
   loop2 lisConf t *)

let recalculTraj = fun vol currentTime plane masse triangulation ->
	let traj = vol.Map.route in
	let backTraj = [] in
	let foreTraj = [] in
	let currentIndex = (currentTime - vol.Map.h_dep) / 5 in
	let rec loop trajloop =
		match trajloop with
		  [] -> ()
		| tete::queue -> if List.length queue > (List.length traj - currentIndex) then
							backTraj = List.append [tete] backTraj
						else 
							foreTraj = List.append [tete] foreTraj;
		loop queue;
	in loop traj;
	let newTraj = Accel.calculTrajectoireTotal foreTraj plane masse triangulation (float currentTime) in
	newTraj = backTraj @ newTraj;
	newTraj;;

let correctionConflits = fun listConf listAv currentTime plane masse triangulation ->
  let listfiltered = ref [] in
  let rec loop listC =
    match listC with
	[] -> ()
      | volConflit::queue -> begin
      	let indexConflit = (currentTime - volConflit.Map.h_dep) / 5 in
	List.nth volConflit.Map.route indexConflit = List.nth volConflit.Map.route (indexConflit - 1); (* peut-être problème avec les vols qui sont toujours en conflit au moment index-1 ==> à vérifier *)
	let stillConflict = ref false in
	
	let rec loopInt listA =
	  match listA with
	      [] -> ()
	    | vol::reste -> if vol != volConflit then
		begin
		  let index = (currentTime - vol.Map.h_dep) / 5 in
		  if  Pente.distance3D (List.nth volConflit.Map.route indexConflit) (List.nth vol.Map.route index) < 5. then
		    stillConflict := true;
		end;
	      loopInt reste;
	in loopInt listAv;

	if (!stillConflict) == false then
	  begin
	  	
	    volConflit.Map.route = (recalculTraj volConflit currentTime plane masse triangulation);(* fonction pour calculer la trajectoire depuis le point courant d'indice index jusqu'au point final ? *)
	    listfiltered := (List.filter (fun i -> i != volConflit) (listC));
	  end;
      end;
	loop (!listfiltered);
  in loop listConf;;

(* pour tous les vols de listeVols faire
     si positionvolCurrentTime = lastPositionvol alors
   retirer vol de listeVols *)

let supprFlights = fun listeAv currentTime ->
  let rec suppr listA =
    match listA with
	[] -> ()
      | vol::queue -> if (currentTime == vol.Map.h_arr) then
	  listeAv := List.filter (fun a -> a != vol) (!listeAv);
	suppr queue
  in suppr (!listeAv);;


let backtrack = fun listFlights currentTime plane masse triangulation ->
  let listeConflits = ref [] in
  let conflit = ref false in
  let listeVols = ref [] in
  
  let rec loop = fun listeFlights currentTime ->
  	listeVols := listeVolsCourrants (!listFlights) currentTime;
  	let (listeConflits, conflit) = detectionConflits conflit (!listeVols) listeConflits currentTime in
  	match (!conflit) with
 		  false -> ()
 		| true -> correctionConflits (!listeConflits) (!listeVols) currentTime plane masse triangulation;
  	supprFlights listeVols currentTime;
   	loop listeVols (currentTime + 5)
      
  in loop listFlights 0;;

(*A modifier :
	- detecter le type d'avion et sa masse directement dans le type flight
	- limiter la profondeur de calcul de la nouvelle traj dans la fonction recalcul traj (/!\ a ce que les la nouvelle traj soit bien continue))

debut de test a modifier :
let list_of_flights = read_file_flights "lfpg_flights.txt" in
let backtrack list_of_flights 0 Accel.a320 Accel.a320.mass_dep triangulation*)