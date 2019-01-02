module Pente = Pente
module Map = Lfpg_map
module Del = Delaunay
module Geo = GeometrieTriangle
module Pivot = Pivot
exception PointFailure;;


(* calcul la nouvelle vitesse de l'avion a partir de l'ancienne dans le cas d'un moteur electrique *)
let nexspeedegts avion mass slope speed =
  let slopetorque = -. mass *. 9.81 *. sin( atan(slope /. 100.)) *. avion.Map.tireradius in
  let restorque = ref 0. in
  let egtstorque = ref 0. in
  if speed < 1. then
     restorque := -.mass *. avion.Map.breakawayresistance *. 10. *. avion.Map.tireradius
  else restorque := -.mass *. avion.Map.rollingresistance *. 10. *. avion.Map.tireradius;
  if speed < 1. then
    egtstorque := avion.Map.maxegtstorque
  else egtstorque := (fun a b -> if a <= b then a else b) avion.Map.maxegtstorque (avion.Map.egtspower /. (speed /. avion.Map.tireradius));
  let aerotorque = avion.Map.aerocoef *. speed *. speed in
  let torque = !egtstorque +. slopetorque +. !restorque +. aerotorque in
  let acc = (fun a b -> if a >= b then a else b) 0. (torque /. avion.Map.tireradius /. mass) in
  speed +. avion.Map.stepcoef *. acc
						    
(* calcul la nouvelle vitesse de l'avion a partir de l'ancienne dans le cas classique *)
let nexspeedclassic speed =
  speed +. 0.9;;


type vect = {x : int; y : int};;

(* calcul le produit vectoriel a partir des vecteurs des droites A1B1 et A2B2 *)
let prod_vect vect_A1B1 vect_A2B2 =
	vect_A1B1.x * vect_A2B2.y - vect_A2B2.x * vect_A1B1.y;;

(* retourne True si il existe un point d'intersection entre les 2 droites *)
let croise_segment pta1 ptb1 pta2 ptb2 =
  let vect_A1B1 = {x=ptb1.Map.x-pta1.Map.x; y=ptb1.Map.y-pta1.Map.y} in
  let vect_A2B2 = {x=ptb2.Map.x-pta2.Map.x; y=ptb2.Map.y-pta2.Map.y} in
  let vect_A1B2 = {x=ptb2.Map.x-pta1.Map.x; y=ptb2.Map.y-pta1.Map.y} in
  let vect_A1A2 = {x=pta2.Map.x-pta1.Map.x; y=pta2.Map.y-pta1.Map.y} in
  let vect_A2B1 = {x=ptb1.Map.x-pta2.Map.x; y=ptb1.Map.y-pta2.Map.y} in
  let vect_A2A1 = {x=pta1.Map.x-pta2.Map.x; y=pta1.Map.y-pta2.Map.y} in
  prod_vect vect_A1B1 vect_A2B2 != 0 &&
      prod_vect vect_A1B1 vect_A1B2 * prod_vect vect_A1B1 vect_A1A2 <= 0 &&
      prod_vect vect_A2B2 vect_A2B1 * prod_vect vect_A2B2 vect_A2A1 <= 0;;

(* calcule le point d'intersection entre la droite ab et le coté cd du triangle englobant a *)
let intersect a b c d tri =
  let pti = ref {Map.x = max_int; Map.y = max_int; Map.z = 0.} in
  let x = ref 0. in
  let y = ref 0. in
  let z = ref 0. in
  let coeff_a_ab,coeff_b_ab = Geo.equadroite a b in
  let coeff_a_cd,coeff_b_cd = Geo.equadroite c d in
  let (i,j,k) = tri.Del.equa in
  if (coeff_a_ab != infinity && coeff_a_cd != infinity)
  then
    begin
      x := ((float (d.Map.y - b.Map.y)) -. (float d.Map.x) *. coeff_a_cd +. (float b.Map.x) *. coeff_a_ab) /. ( coeff_a_ab -. coeff_a_cd );
      y := coeff_a_ab *. ( !x -. (float a.Map.x)) +. (float a.Map.y);
      z := i *. !x +. j *. !y +. k;
      pti := {Map.x=int_of_float !x; Map.y=int_of_float !y; Map.z= !z};
    end
  else
    if (coeff_a_ab = infinity && coeff_a_cd != infinity)
    then
      begin
	x := float a.Map.x;
	y := coeff_a_cd *. ( !x -. (float c.Map.x)) +. (float c.Map.y);
	z := i *. !x +. j *. !y +. k;
	pti := {Map.x= a.Map.x; Map.y=int_of_float !y; Map.z= !z};
      end
    else
      if (coeff_a_ab != infinity && coeff_a_cd = infinity)
      then
	begin
	  x := float c.Map.x;
	  y := coeff_a_ab *. ( !x -. (float a.Map.x)) +. (float a.Map.y);
	  z := i *. !x +. j *. !y +. k;
	  pti := {Map.x= c.Map.x; Map.y=int_of_float !y; Map.z= !z};
	end;
  !pti;;

(* calcul le point d'intersection (ou garde l'infini) a partir d'un point de depart, du triangle entourant ce point, et d'un point d'arrivé *)
let croise_tri pt_dep pt_arr tri =
  let pti = ref ({Map.x= max_int;Map.y= max_int;Map.z=0.}) in
  if not (Geo.point_dans_triangle pt_arr tri)
  then
    begin
      pti := {Map.x= max_int;Map.y= max_int;Map.z=1.};
      if croise_segment tri.Del.p1 tri.Del.p2 pt_dep pt_arr
      then
	pti := intersect tri.Del.p1 tri.Del.p2 pt_dep pt_arr tri;
      if croise_segment tri.Del.p1 tri.Del.p3 pt_dep pt_arr
      then
	pti := intersect tri.Del.p1 tri.Del.p3 pt_dep pt_arr tri;
      if croise_segment tri.Del.p2 tri.Del.p3 pt_dep pt_arr
      then
	pti := intersect tri.Del.p2 tri.Del.p3 pt_dep pt_arr tri;
    end;
  (!pti);;

(* permet de prendre le bon triangle (dans le cas où on est pas dans le bon triangle le point d'intersection avec celui ci serait notre point de depart) pour considerer un point d'intersection correct
renvoie donc le point d'intersection vérifié ou le point d'arrivé dans le cas où il n'y a pas d'intersection *)
let bonneintersection = fun dep arriv listeDelaunay  ->
  let listeTriangle = Geo.dansQuelTriangle dep listeDelaunay in
  let ptcorrec = ref ({Map.x= max_int;Map.y= max_int;Map.z= 0.}) in
  let ptcomparaison = {Map.x= max_int;Map.y= max_int ;Map.z=1.} in
  let ptref = {Map.x= max_int;Map.y= max_int;Map.z=0.} in
  let rec loop = fun listeTriangle ->
    match listeTriangle with
	[] -> ();
      | triangle::reste ->
	let pt = croise_tri dep arriv triangle in
	if (pt != ptcomparaison && pt != ptref)
	then ptcorrec := pt;
	loop reste;
  in loop listeTriangle;
  if (!ptcorrec = ptref || !ptcorrec = ptcomparaison || !ptcorrec = dep)
  then ptcorrec := arriv;
  (!ptcorrec);;


(* calcul de la nouvelle vitesse a partir de 2 points connus appartenant à un même triangle ev verifiant si c'est un moteur electriue ou non *)
let new_speed = fun depart inter avion masse speedavant speedmax flight_stand  ->
  let newspeed = ref 0. in
  let distance = Pente.distance3D depart inter in
  let slope = (depart.Map.z -. inter.Map.z) /. distance in
  let nextspeed = ref 0. in
  begin
    if flight_stand = "E"
    then
      nextspeed := nexspeedegts avion masse slope speedavant
    else
      nextspeed := nexspeedclassic speedavant;
    if !nextspeed <= speedmax
    then
      newspeed := !nextspeed
    else
      newspeed := speedmax;
    if speedmax >= 17. (* on est certainement sur la piste en train d'accelerer ou de décelerer donc on n'utilise plus les moteurs electriques*)
    then
      newspeed := speedmax;
  end;
  !newspeed ;;

(* calcul et revoie le temps pour parcourir la distante entre 2 points à la vitesse calculée par le modéle choisi *)
let temps2point1triangle = fun pointdep point avion masse vitesseAvant vitesseMax flight_stand ->
  let vitesseElec = new_speed pointdep point avion masse vitesseAvant vitesseMax flight_stand in
  let distance = Pente.distance3D pointdep point in
  let timeElect = distance /. vitesseElec in
  timeElect;;

(* renvoie le point situé à la distance du point de depart tel que à la vitesse choisie, un temps t a été parcouru *)
let distanceParcourue = fun pointdep point temps avion masse vitesseAvant vitesseMax flight_stand ->
  let vitesseElec = new_speed pointdep point avion masse vitesseAvant vitesseMax flight_stand in
  let distanceAparcourir = vitesseElec *. temps in
  let pointAGarder = Geo.intersecSegCercle pointdep point distanceAparcourir in
   pointAGarder;;

(* calcul de l'altitude d'un point *)
let calculAltitudePoint = fun point triangulation ->
  let listeTriangle = Geo.dansQuelTriangle point triangulation in
  match listeTriangle with
      [] -> 0.;
    | triangle::reste -> Pente.altitudePoint point triangle;;

(* renvoie la liste des points espacés de 5s pour la nouvelle trajectoire entre 2 points *)
let  calculTrajectoireEntre2points = fun pointdep pointarriv avion masse triangulation compteurTempsA5s timeSimulation vitesseAvant flight_stand ->
  let listePointAGarder = ref [] in
  let vitesseNoElec = Pente.vitesse_5s_3d pointdep pointarriv in
  let rec loop = fun point1 ->
    match point1 with
      | a when a = pointarriv -> ()
      | _ ->
	let pointintersec = bonneintersection point1 pointarriv triangulation in
	let timeIntersec = temps2point1triangle point1 pointintersec avion masse !vitesseAvant vitesseNoElec flight_stand in
	begin
	  compteurTempsA5s := (!compteurTempsA5s) +. timeIntersec;
	  let rec loopTantQueCompteurSup5 = fun compteurSup pointGarde ->
	    match compteurSup with
	      | a when a < timeSimulation -> compteurTempsA5s := a;
	      | compteur when compteur >= timeSimulation && compteur < (2. *. timeSimulation) ->
		let pointAgarder = distanceParcourue pointGarde pointintersec (compteur -. timeSimulation) avion masse !vitesseAvant vitesseNoElec flight_stand in
		begin
		  listePointAGarder := List.append (!listePointAGarder) (pointAgarder::[]);
		  compteurTempsA5s := compteur -. timeSimulation;
		  vitesseAvant := new_speed pointGarde pointintersec avion masse !vitesseAvant vitesseNoElec flight_stand;
		end;
	      | compteur ->
		let pointAGarder = distanceParcourue pointGarde pointintersec timeSimulation avion masse !vitesseAvant vitesseNoElec flight_stand in
		begin
		  listePointAGarder := List.append (!listePointAGarder) (pointAGarder::[]);
		  compteurTempsA5s := compteur -. timeSimulation;
		  vitesseAvant := new_speed pointGarde pointintersec avion masse !vitesseAvant vitesseNoElec flight_stand;
		end;
		loopTantQueCompteurSup5 (compteur -. timeSimulation) pointAGarder;
	  in loopTantQueCompteurSup5 !compteurTempsA5s point1;
	end;
	loop pointintersec;
  in loop pointdep;
  if (pointdep = pointarriv)
  then
    vitesseAvant := 0.;
  (!listePointAGarder);;

(* calcul de la trajectoire totale a partir de la liste de point initiale et de la triangulation de Delaunay *)
let calculTrajectoireTotal = fun trajectoireInitiale avion masse triangulation timeSimulation flight_stand ->
  let compteurTempsA5s = ref 0. in
  let trajectoireElectrique = ref [] in
  let vitesseAvant = ref 0. in
  begin
  match trajectoireInitiale with
    | [] -> ()
    | pointdebut::reste ->
      	let zdep  = calculAltitudePoint pointdebut triangulation in
	let point  = {Map.x = pointdebut.Map.x; Map.y = pointdebut.Map.y; Map.z = zdep} in
	trajectoireElectrique := point::[];
  end;
  let rec loop = fun listePoints ->
    match listePoints with
      | [] -> ()
      | [a] ->
	if (!compteurTempsA5s) >= 3.
	then
	  begin
	    let z = calculAltitudePoint a triangulation in
	    let  b = {Map.x = a.Map.x; Map.y = a.Map.y; Map.z = z} in
	    trajectoireElectrique := List.append (!trajectoireElectrique) (b::[])
	  end;
      | pointdep::pointarriv::reste ->
	let zdep  = calculAltitudePoint pointdep triangulation in
	let depAlti = {Map.x = pointdep.Map.x; Map.y = pointdep.Map.y; Map.z = zdep} in
	let zarr  = calculAltitudePoint pointarriv triangulation in
	let arrAlti = {Map.x = pointarriv.Map.x; Map.y = pointarriv.Map.y; Map.z = zarr} in
	let listeAajouter = calculTrajectoireEntre2points depAlti arrAlti avion masse triangulation compteurTempsA5s timeSimulation vitesseAvant flight_stand in
	trajectoireElectrique := List.append (!trajectoireElectrique) listeAajouter;
	loop (pointarriv::reste);
  in loop trajectoireInitiale;
  
  (!trajectoireElectrique);;


(*

(* test à faire pour verifier les points obtenus *)
(* a verifier avec la vision des trajectoires *)
let p1={Map.x= -3787;Map.y=519;Map.z=0.}
let p5={Map.x= -3787;Map.y=519;Map.z=0.}
let p2={Map.x= -3799;Map.y=524;Map.z=0.}
let p3={Map.x= -3812;Map.y=531;Map.z=0.}
let p4={Map.x= -3825;Map.y=539;Map.z=0.}
let traj=p1::p5::p5::p5::p5::p2::p2::p2::p2::p3::p4::[]

let trajet = [ 
  {Map.x = -738 ; y = 493 ; z =0.000000}; 
{Map.x = -750 ; y = 500 ; z =0.000000}; 
{Map.x = -764 ; y = 508 ; z =0.000000}; 
{Map.x = -777 ; y = 514 ; z =0.000000}; 
{Map.x = -790 ; y = 521 ; z =0.000000}; 
{Map.x = -803 ; y = 528 ; z =0.000000}; 
{Map.x = -816 ; y = 536 ; z =0.000000}; 
{Map.x = -828 ; y = 545 ; z =0.000000}; 
{Map.x = -838 ; y = 556 ; z =0.000000}; 
{Map.x = -846 ; y = 568 ; z =0.000000}; 
{Map.x = -850 ; y = 583 ; z =0.000000}; 
{Map.x = -850 ; y = 583 ; z =0.000000}; 
{Map.x = -850 ; y = 583 ; z =0.000000}; 
{Map.x = -850 ; y = 583 ; z =0.000000}; 
{Map.x = -850 ; y = 583 ; z =0.000000}; 
{Map.x = -850 ; y = 583 ; z =0.000000}; 
{Map.x = -850 ; y = 583 ; z =0.000000}; 
{Map.x = -850 ; y = 583 ; z =0.000000}; 
{Map.x = -850 ; y = 583 ; z =0.000000}; 
{Map.x = -850 ; y = 583 ; z =0.000000}; 
{Map.x = -850 ; y = 583 ; z =0.000000}; 
{Map.x = -850 ; y = 583 ; z =0.000000}; 
{Map.x = -850 ; y = 583 ; z =0.000000};
{Map.x = -850 ; y = 583 ; z =0.000000}; 
{Map.x = -850 ; y = 583 ; z =0.000000}; 
{Map.x = -850 ; y = 583 ; z =0.000000}; 
{Map.x = -850 ; y = 583 ; z =0.000000}; 
{Map.x = -850 ; y = 583 ; z =0.000000}; 
{Map.x = -850 ; y = 583 ; z =0.000000}; 
{Map.x = -850 ; y = 533 ; z =0.000000}; 
{Map.x = -847 ; y = 483 ; z =0.000000}; 
{Map.x = -840 ; y = 438 ; z =0.000000}; 
{Map.x = -829 ; y = 395 ; z =0.000000}; 
{Map.x = -812 ; y = 353 ; z =0.000000}; 
{Map.x = -793 ; y = 313 ; z =0.000000}; 
{Map.x = -771 ; y = 279 ; z =0.000000}; 
{Map.x = -744 ; y = 250 ; z =0.000000}; 
{Map.x = -711 ; y = 228 ; z =0.000000}; 
{Map.x = -672 ; y = 218 ; z =0.000000}; 
{Map.x = -632 ; y = 216 ; z =0.000000}; 
{Map.x = -592 ; y = 219 ; z =0.000000}; 
{Map.x = -553 ; y = 223 ; z =0.000000}; 
{Map.x = -503 ; y = 228 ; z =0.000000}; 
{Map.x = -454 ; y = 233 ; z =0.000000};
{Map.x = -404 ; y = 238 ; z =0.000000}; 
{Map.x = -354 ; y = 243 ; z =0.000000}; 
{Map.x = -304 ; y = 248 ; z =0.000000}; 
{Map.x = -254 ; y = 253 ; z =0.000000}; 
{Map.x = -204 ; y = 257 ; z =0.000000}; 
{Map.x = -154 ; y = 261 ; z =0.000000}; 
{Map.x = -105 ; y = 265 ; z =0.000000};
{Map.x = -55 ; y = 270 ; z =0.000000}; 
{Map.x = -5 ; y = 276 ; z =0.000000}; 
{Map.x = 27 ; y = 289 ; z =0.000000};
{Map.x = 51 ; y = 313 ; z =0.000000};
{Map.x = 73 ; y = 340 ; z =0.000000}; 
{Map.x = 87 ; y = 376 ; z =0.000000}; 
{Map.x = 99 ; y = 415 ; z =0.000000};
{Map.x = 102 ; y = 455 ; z =0.000000}; 
{Map.x = 96 ; y = 494 ; z =0.000000}; 
{Map.x = 89 ; y = 544 ; z =0.000000}; 
{Map.x = 85 ; y = 594 ; z =0.000000}; 
{Map.x = 80 ; y = 644 ; z =0.000000}; 
{Map.x = 76 ; y = 694 ; z =0.000000}; 
{Map.x = 72 ; y = 744 ; z =0.000000}; 
{Map.x = 68 ; y = 794 ; z =0.000000}; 
{Map.x = 63 ; y = 844 ; z =0.000000}; 
{Map.x = 60 ; y = 894 ; z =0.000000}; 
{Map.x = 55 ; y = 943 ; z =0.000000}; 
{Map.x = 49 ; y = 993 ; z =0.000000}; 
{Map.x = 44 ; y = 1043 ; z =0.000000}; 
{Map.x = 42 ; y = 1083 ; z =0.000000}; 
{Map.x = 47 ; y = 1102 ; z =0.000000}; 
{Map.x = 55 ; y = 1121 ; z =0.000000}; 
{Map.x = 70 ; y = 1147 ; z =0.000000}; 
{Map.x = 94 ; y = 1164 ; z =0.000000}; 
{Map.x = 123 ; y = 1169 ; z =0.000000}; 
{Map.x = 153 ; y = 1172 ; z =0.000000}; 
{Map.x = 198 ; y = 1179 ; z =0.000000}; 
{Map.x = 243 ; y = 1186 ; z =0.000000}; 
{Map.x = 288 ; y = 1192 ; z =0.000000}; 
{Map.x = 338 ; y = 1196 ; z =0.000000}; 
{Map.x = 388 ; y = 1199 ; z =0.000000}; 
{Map.x = 438 ; y = 1203 ; z =0.000000}; 
{Map.x = 488 ; y = 1207 ; z =0.000000}; 
{Map.x = 538 ; y = 1212 ; z =0.000000}; 
{Map.x = 588 ; y = 1220 ; z =0.000000}; 
{Map.x = 633 ; y = 1227 ; z =0.000000}; 
{Map.x = 678 ; y = 1232 ; z =0.000000}; 
{Map.x = 723 ; y = 1233 ; z =0.000000}; 
{Map.x = 768 ; y = 1232 ; z =0.000000}; 
{Map.x = 813 ; y = 1234 ; z =0.000000}; 
{Map.x = 856 ; y = 1244 ; z =0.000000}; 
{Map.x = 873 ; y = 1263 ; z =0.000000}; 
{Map.x = 877 ; y = 1288 ; z =0.000000}; 
{Map.x = 876 ; y = 1313 ; z =0.000000}; 
{Map.x = 874 ; y = 1338 ; z =0.000000}; 
{Map.x = 870 ; y = 1388 ; z =0.000000}; 
{Map.x = 864 ; y = 1438 ; z =0.000000}; 
{Map.x = 856 ; y = 1461 ; z =0.000000}; 
{Map.x = 839 ; y = 1478 ; z =0.000000}; 
{Map.x = 814 ; y = 1485 ; z =0.000000}; 
{Map.x = 807 ; y = 1485 ; z =0.000000}; 
{Map.x = 784 ; y = 1484 ; z =0.000000}; 
{Map.x = 714 ; y = 1478 ; z =0.000000}; 
{Map.x = 598 ; y = 1468 ; z =0.000000}; 
{Map.x = 435 ; y = 1454 ; z =0.000000}; 
{Map.x = 226 ; y = 1436 ; z =0.000000}; 
{Map.x = -31 ; y = 1414 ; z =0.000000}; 
{Map.x = -333 ; y = 1389 ; z =0.000000}; 
{Map.x = -682 ; y = 1359 ; z =0.000000}; 
{Map.x = -1078 ; y = 1326 ; z =0.000000}; 
{Map.x = -1520 ; y = 1288 ; z =0.000000}; 
{Map.x = -2009 ; y = 1247 ; z =0.000000}; 
{Map.x = -2545 ; y = 1201 ; z =0.000000}; ]
  
(*
let trajet = [{Map.x = 51 ; y = 313 ; z =0.000000};
{Map.x = 73 ; y = 340 ; z =0.000000}; 
{Map.x = 87 ; y = 376 ; z =0.000000};
{Map.x = 99 ; y = 415 ; z =0.000000};
{Map.x = 102 ; y = 455 ; z =0.000000}; ]
  *)

let point1 = {Map.x = 51 ; y = 313 ; z =104.3}
let point2 = {Map.x = 73 ; y = 340 ; z =104.2}
(*
let trajet = point1::point2::[]
  *)
let delau = Pivot.triangle_equa 
let time = 5.
let masse = a320.mass_dep

let trajectoire = calculTrajectoireTotal trajet a320 masse delau time ;;


let () =
List.iter (fun i -> Printf.printf "\ntrajectoire point %d %d %f \n" i.Map.x i.Map.y i.Map.z) trajectoire;;

*)
