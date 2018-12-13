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

let a319 = {tipe = "a320";
 	mass_dep = 63000.;
 	mass_arri = 57000.;
 	tireradius = 0.56;
	maxegtstorque = 16000.;
 	egtspower = 46000.;
 	breakawayresistance = 0.01;
	rollingresistance = 0.007;
	aerocoef = 1.032;
	stepcoef = 4.1};;

let a321 = {tipe = "a320";
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
  if not (Pente.point_dans_triangle pt_arr tri)
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
  let ptcorrec = ref ({Map.x=0,Map.y=0;Map.z=0.}) in
  let ptcomparaison = {Map.x=0;Map.y=0;Map.z=1.0} in
  let ptref = {Map.x=0;Map.y=0;Map.z=0.0} in
  let rec loop = fun listeTriangle ->
    match listeTriangle with
	[] -> ();
      | triangle::reste -> pt = croise_tri dep arriv triangle;
	if (pt != ptcomparaison && pt != ptref)
	then ptcorrec := pt;
	loop reste;
  in loop listeTriangle;
  if (!ptcorrec = ptref)
  then ptcorrec := arriv;
  (!ptcorrec);;

(* on change la maniere de procéder :
il faut calculer la position où on se trouvera au bout d'un temps t 
entre 2 points
si 2 fois le meme -> passe au suivant (le electrique n'attend pas)
si pas 2 fois le meme -> on calcul entre les 2 la nouvelle position
   il faut penser à noter le temps que prendra le "reste" du segment pour relancer correctement sur les 2 points suivants
*)



(* calcul de la nouvelle vitesse a partir de 2 points connus appartenant à un même triangle *)
(* ici calcul avec les moteurs electriques *)
let new_speed = fun depart inter avion masse speed->
  let distance = Pente.distance2d depart inter in
  let slope = (depart.Map.z -. inter.Map.z) /. distance in
  let nextspeed = nexspeedegts avion masse slope speed in
  nextspeed ;;

let temps2point1triangle = fun pointdep point avion masse vitesseAvant ->
  let vitesseElec = new_speed pointdep point avion masse vitesseAvant  in
  let distance = Pente.distance3D pointdep point in
  let timeElect = distance /. vitesseElec in
  timeElect;;

let distanceParcourue = fun pointdep point temps ->
  let vitesseElec = new_speed pointdep point avion masse vitesseAvant  in
  let distanceAparcourir = vitesseElec *. temps in
  let pointAGarder = Geo.intersecSegCercle pointdep point distanceAParcourir in
   pointAGarder;;

  
let  calculTrajectoireEntre2points = fun pointdep pointarriv avion masse triangulation compteurTempsA5s timeSimulation ->
  let listePointAGarder = ref [] in
  let newCompteur =  ref 0.  in
  let vitesseNoElec = Geo.vitesse_5s_3d pointdep pointarriv in
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
	      List.append (!listePointAGarder) ((distanceParcourue point1 pointintersec (timeSimulation -. (!compteurTempsA5s)))::[]);
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
  let (pointdebut::reste) = trajectoireInitiale in
  trajectoireElectrique := pointdebut::[];
  let loop = fun listePoints ->
    match listePoints with
      | [a] ->
	if (!compteurTempsA5s) >= 3.
	then
	    List.append (!trajectoireElectrique) (a::[]);
      | pointdep::pointarriv::reste ->
	let listeAajouter = calculTrajectoireEntre2points pointdep pointarriv avion masse triangulation compteurTempsA5s timeSimulation in
	List.append (!trajectoireElectrique) listeAajouter;
	loop (pointarriv::reste);
  in loop trajectoireInitiale;
  (!trajectoireElectrique);;


(* test à faire pour verifier les points obtenus *)
