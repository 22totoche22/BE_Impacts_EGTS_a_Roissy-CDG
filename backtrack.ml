module Pente = Pente

(* fun detectionConflits : *)
(* tous les avions avancent de 5s 
   pour tous les avions i de listeAvions faire
     pour tous les avions j de avionI::resteListeAvions faire
       si positionCouranteAvionI == positionCouranteAvionJ alors
         conf = true;
         listeConflits = listeConflits::avionI; *)
let detectionConflits = fun conf listeAvions listConf index ->
  let rec loop listeAv =
    match listeAv with
	[] -> ()
      | avion::queue -> let rec loopInt av liste =
			  match queue with
			      [] -> ()
			    | avionCompare::reste -> if Pente.distance3D avion.route[index] avionCompare.route[index] < 5 then
				begin
				  conf := true;
				  listConf = listConf::avion;
				end;
			      loopInt avion reste in
			loopInt avion queue;
			loop queue in
  loop listeAvions;
  listConf;;


(* pour tous les avions de listConf faire
     pour tous les avions i de ListAvions otée de ListConf faire
       positionAvionConflitT = positionAvionConflitT-5s;
       si positionAvionConflitT = positionAvionIT alors
         recalcul de la trajetoire de avionConflit
         retirer avionConf de listConf
   loop2 lisConf t *)
let correctionConflits = fun listConf listAv index ->
  let rec loop listC =
    match listC with
	[] -> ()
      | avionConflit::queue -> begin
	avionConflit.route[index] = avionConflit.route[index - 1];
	let stillConflict = false in
	
	let rec loopInt listA =
	  match listAv with
	      [] -> ()
	    | avion::reste -> if avion != avionConflit then
		if  Pente.distance3D avionConflit.route[index] avion.route[index] < 5 then
		  stillConflict = true;
	      loopInt reste;
	in loopInt listAv;
	
	if stillConflict == false then
	  begin
	    avionConflit.route = (* fonction pour calculer la trajectoire depuis le point courant d'indice index jusqu'au point final ? *);
	    listConf = List.filter (a != avionConflit) listConf;
	  end;
      end;
	loop listC;
  in loop listConf;;
	
	    


let backtrack = fun ->
  let listeConflits = [] in
  let conflit = ref false in
  let indexTraj = currentTime / 5 + 1 in

  let rec loop = fun conf listeAvions indexTraj ->
    while conf = false do
      listeConflits = detectionConflits (!conflit) listeAvions listeConflits indexTraj;
    done;
    
    let rec loop2 = fun listConf, t ->
      correctionConflits = fun listConflits listAvions indexTraj;
    in loop2 listeConflits currentTime

    (* pour tous les avions de listeAvions faire
         si positionAvionCurrentTime = lastPositionAvion alors
       retirer avion de listeAvions *)

    loop conf listeAvions (currentTime + 5)
  in loop conflit listFlight 0;;
