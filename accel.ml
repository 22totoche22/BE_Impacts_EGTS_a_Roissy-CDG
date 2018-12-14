module Pente = Pente
module Map = Lfpg_map
module Del = Delaunay
module Geo = GeometrieTriangle;;

exception PointFailure;;

type avion = {tipe : string;
	      mass_dep : float;
	      mass_arri : float;
	      tireradius : float;
	      maxegtstorque : float;
	      egtspower : float;
	      breakawayresistance : float;
	      rollingresistance : float;
	      aerocoef : float;
	      stepcoef : float}


let nexspeedegts avion mass slope speed =
  let slopetorque = -. mass *. 9.81 *. sin( atan(slope /. 100.)) *. avion.tireradius in
  let restorque = ref 0. in
  let egtstorque = ref 0. in
  if speed < 1. then
     restorque := -.mass *. avion.breakawayresistance *. 10. *. avion.tireradius
  else restorque := -.mass *. avion.rollingresistance *. 10. *. avion.tireradius;
  if speed < 1. then
    egtstorque := avion.maxegtstorque
  else egtstorque := (fun a b -> if a <= b then a else b) avion.maxegtstorque (avion.egtspower /. (speed /. avion.tireradius));
  let aerotorque = avion.aerocoef *. speed *. speed in
  let torque = !egtstorque +. slopetorque +. !restorque +. aerotorque in
  let acc = (fun a b -> if a >= b then a else b) 0. (torque /. avion.tireradius /. mass) in
  speed +. avion.stepcoef *. acc
						    

let nexspeedclassic speed =
  speed +. 0.9;;

let a320 = {tipe = "a320";
 	mass_dep = 69000.;
 	mass_arri = 62000.;
 	tireradius = 0.56;
	maxegtstorque = 16000.;
 	egtspower = 46000.;
 	breakawayresistance = 0.01;
	rollingresistance = 0.007;
	aerocoef = 1.032;
	stepcoef = 4.1};;

let a319 = {tipe = "a319";
 	mass_dep = 63000.;
 	mass_arri = 57000.;
 	tireradius = 0.56;
	maxegtstorque = 16000.;
 	egtspower = 46000.;
 	breakawayresistance = 0.01;
	rollingresistance = 0.007;
	aerocoef = 1.032;
	stepcoef = 4.1};;

let a321 = {tipe = "a321";
 	mass_dep = 81000.;
 	mass_arri = 73000.;
 	tireradius = 0.56;
	maxegtstorque = 16000.;
 	egtspower = 46000.;
 	breakawayresistance = 0.01;
	rollingresistance = 0.007;
	aerocoef = 1.032;
	stepcoef = 4.1};;

type vect = {x : int; y : int};;

let prod_vect vect_A1B1 vect_A2B2 =
	vect_A1B1.x * vect_A2B2.y - vect_A2B2.x * vect_A1B1.y;;
 
let croise_segment pta1 ptb1 pta2 ptb2 =
  let vect_A1B1 = {x=pta1.Map.x-ptb1.Map.x; y=pta1.Map.y-ptb1.Map.y} in
  let vect_A2B2 = {x=pta2.Map.x-ptb2.Map.x; y=pta2.Map.y-ptb2.Map.y} in
  let vect_A1B2 = {x=pta1.Map.x-ptb2.Map.x; y=pta1.Map.y-ptb2.Map.y} in
  let vect_A1A2 = {x=pta1.Map.x-pta2.Map.x; y=pta1.Map.y-pta2.Map.y} in
  let vect_A2B1 = {x=pta2.Map.x-ptb1.Map.x; y=pta2.Map.y-ptb1.Map.y} in
  let vect_A2A1 = {x=pta2.Map.x-pta1.Map.x; y=pta2.Map.y-pta1.Map.y} in
  prod_vect vect_A1B1 vect_A2B2 != 0 &&
      prod_vect vect_A1B1 vect_A1B2 * prod_vect vect_A1B1 vect_A1A2 <= 0 &&
      prod_vect vect_A2B2 vect_A2B1 * prod_vect vect_A2B2 vect_A2A1 <= 0;;

let intersect a b c d tri =
    let xi = (((b.Map.y-a.Map.y)/(b.Map.x-a.Map.x))*a.Map.x+a.Map.y-((d.Map.y-c.Map.y)/(d.Map.x-c.Map.x))*c.Map.x-c.Map.y)/(((b.Map.y-a.Map.y)/(b.Map.x-a.Map.x))-((d.Map.y-c.Map.y)/(d.Map.x-c.Map.x))) in
    let yi = ((b.Map.y-a.Map.y)/(b.Map.x-a.Map.x))*((((b.Map.y-a.Map.y)/(b.Map.x-a.Map.x))*a.Map.x+a.Map.y-((d.Map.y-c.Map.y)/(d.Map.x-c.Map.x))*c.Map.x-c.Map.y)/(((b.Map.y-a.Map.y)/(b.Map.x-a.Map.x))-((d.Map.y-c.Map.y)/(d.Map.x-c.Map.x)))-a.Map.x)+a.Map.y in
    let (i,j,k) = tri.Del.equa in
    let zi = i *. (float xi) +. j *. (float yi) +. k in
    let pti = {Map.x=xi; Map.y=yi; Map.z=zi} in
    pti;;
  
let croise_tri pt_dep pt_arr tri =
  let pti = ref ({Map.x=0;Map.y=0;Map.z=0.}) in
  if not (Geo.point_dans_triangle pt_arr tri)
  then
    begin
      pti := {Map.x=0;Map.y=0;Map.z=1.0};
      if croise_segment tri.Del.p1 tri.Del.p2 pt_dep pt_arr
      then pti := intersect tri.Del.p1 tri.Del.p2 pt_dep pt_arr tri;
      if croise_segment tri.Del.p2 tri.Del.p3 pt_dep pt_arr
      then pti := intersect tri.Del.p2 tri.Del.p3 pt_dep pt_arr tri;
      if croise_segment tri.Del.p1 tri.Del.p3 pt_dep pt_arr
      then pti := intersect tri.Del.p1 tri.Del.p3 pt_dep pt_arr tri;
    end;
  (!pti);;

(* permet de prendre le bon triangle (dans le cas où on est pas dans le bon triangle le point d'intersection avec celui ci serait notre point de depart *)
let bonneintersection = fun dep arriv listeDelaunay  ->
  let listeTriangle = Geo.dansQuelTriangle dep listeDelaunay in
  let ptcorrec = ref ({Map.x=0;Map.y=0;Map.z=0.}) in
  let ptcomparaison = {Map.x=0;Map.y=0;Map.z=1.0} in
  let ptref = {Map.x=0;Map.y=0;Map.z=0.0} in
  let rec loop = fun listeTriangle ->
    match listeTriangle with
	[] -> ();
      | triangle::reste ->
	let pt = croise_tri dep arriv triangle in
	if (pt != ptcomparaison && pt != ptref)
	then ptcorrec := pt;
	loop reste;
  in loop listeTriangle;
  if (!ptcorrec = ptref || !ptcorrec = ptcomparaison)
  then ptcorrec := arriv;
  (!ptcorrec);;


(* calcul de la nouvelle vitesse a partir de 2 points connus appartenant à un même triangle *)
(* ici calcul avec les moteurs electriques *)
let new_speed = fun depart inter avion masse speed->
  let distance = Pente.distance2D depart inter in
  let slope = (depart.Map.z -. inter.Map.z) /. distance in
  let nextspeed = nexspeedegts avion masse slope speed in
  nextspeed ;;

let temps2point1triangle = fun pointdep point avion masse vitesseAvant ->
  let vitesseElec = new_speed pointdep point avion masse vitesseAvant  in
  let distance = Pente.distance3D pointdep point in
  let timeElect = distance /. vitesseElec in
  timeElect;;

let distanceParcourue = fun pointdep point temps avion masse vitesseAvant ->
  let vitesseElec = new_speed pointdep point avion masse vitesseAvant  in
  let distanceAparcourir = vitesseElec *. temps in
  let pointAGarder = Geo.intersecSegCercle pointdep point distanceAparcourir in
   pointAGarder;;

  
let  calculTrajectoireEntre2points = fun pointdep pointarriv avion masse triangulation compteurTempsA5s timeSimulation ->
  let listePointAGarder = ref [] in
  let newCompteur =  ref 0.  in
  let vitesseNoElec = Pente.vitesse_5s_3d pointdep pointarriv in
  let rec loop = fun point1 ->
    match point1 with
      | a when a = pointarriv -> ()
      | _ ->
	let pointintersec = bonneintersection point1 pointarriv triangulation in
	let timeIntersec = temps2point1triangle point1 pointintersec avion masse vitesseNoElec in
	begin
	  newCompteur := (!compteurTempsA5s) +. timeIntersec;
	  if ((!newCompteur) >= timeSimulation)
	  then
	    begin
	      let pointAgarder = distanceParcourue point1 pointintersec (timeSimulation -. (!compteurTempsA5s)) avion masse vitesseNoElec in
	      listePointAGarder := List.append (!listePointAGarder) (pointAgarder::[]);
	      compteurTempsA5s := (!newCompteur) -. timeSimulation;
	    end
	  else
	    compteurTempsA5s := (!newCompteur);
	end;
	loop pointintersec;
  in loop pointdep;
  (!listePointAGarder);;

let calculTrajectoireTotal = fun trajectoireInitiale avion masse triangulation timeSimulation ->
  let compteurTempsA5s = ref 0. in
  let trajectoireElectrique = ref [] in
  begin
  match trajectoireInitiale with
    | [] -> ()
    | pointdebut::reste -> trajectoireElectrique := pointdebut::[];
  end;
  let rec loop = fun listePoints ->
    match listePoints with
      | [] -> ()
      | [a] ->
	if (!compteurTempsA5s) >= 3.
	then
	    trajectoireElectrique := List.append (!trajectoireElectrique) (a::[]);
      | pointdep::pointarriv::reste ->
	let listeAajouter = calculTrajectoireEntre2points pointdep pointarriv avion masse triangulation compteurTempsA5s timeSimulation in
	trajectoireElectrique := List.append (!trajectoireElectrique) listeAajouter;
	loop (pointarriv::reste);
  in loop trajectoireInitiale;
  (!trajectoireElectrique);;



(*
(* test à faire pour verifier les points obtenus *)
(* a verifier avec la vision des trajectoires *)
let p1={Map.x= -3787;Map.y=519;Map.z=0.}
let p2={Map.x= -3799;Map.y=524;Map.z=100.}
let p3={Map.x= -3812;Map.y=531;Map.z=200.}
let p4={Map.x= -3825;Map.y=539;Map.z=300.}
let traj=p1::p2::p3::p4::[]
let delau = Del.listeTriangle
let time = 5.
let masse = a320.mass_dep

let trajectoire = calculTrajectoireTotal traj a320 masse delau time;;

List.iter (fun i -> Printf.printf "\ntrajectoire point %d %d %f \n" i.Map.x i.Map.y i.Map.z) trajectoire;;
*)
