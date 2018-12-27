module Map = Lfpg_map

  (* type triangle = 3 points + 1 equation de plan *)
type triangle ={p1: Map.point; p2: Map.point; p3: Map.point; mutable equa: float*float*float};;


(* retourne le point ayant le x le plus petit *)
let minimum = fun x1 x2 x3 y1 y2 y3->
  if (x1 <= x2 && x1 <= x3)
  then
    x1,y1
  else
    if (x2 <= x1 && x2 <= x3)
    then
      x2,y2
    else
      x3,y3;;

(* retourne le point ayant le x le plus grand *)
let maximum = fun x1 x2 x3 y1 y2 y3->
  if (x1 >= x2 && x1 >= x3)
  then
    x1,y1
  else
    if (x2 >= x1 && x2 >= x3)
    then
      x2,y2
    else
      x3,y3;;

(* calcule le centre (x et y) du cercle circonscrit à un triangle et son rayon *)
let cercle_circonscrit = fun triangle ->
  let x1 = triangle.p1.Map.x in
  let y1 = triangle.p1.Map.y in
  let x2 = triangle.p2.Map.x in
  let y2 = triangle.p2.Map.y in
  let x3 = triangle.p3.Map.x in
  let y3 = triangle.p3.Map.y in
  (* a et b tel que pour la droite passant par pNpM y = a*x + b *)
  let ap1p2 = ref 0. in
  let ap2p3 = ref 0. in
  let bp1p2 = ref 0. in
  let bp2p3 = ref 0. in
  let xC = ref 0. in
  let yC = ref 0. in
  let rayon = ref 0. in
  begin
    (* cas général *)
    ap1p2 := (float (-(x2 - x1))) /. (float (y2 - y1));
    bp1p2 := (float (x2 * x2 - x1 * x1 + y2 * y2 - y1 * y1)) /. (float (2 * (y2 - y1)));
    ap2p3 := (float (-(x3 - x2))) /. (float (y3 - y2));
    bp2p3 := (float (x3 * x3 - x2 * x2 + y3 * y3 - y2 * y2)) /. (float (2 * (y3 - y2)));
    (* cas particulier coté p1p2  paralléle à l'axe des abscisses *)
    if (y2 - y1) = 0
    then
      begin
	(* on prendre la droite p1p3 à la place *)
	ap1p2 := (float (-(x3 - x1))) /. (float (y3 - y1));
	bp1p2 := (float (x3 * x3 - x1 * x1 + y3 * y3 - y1 * y1)) /. (float (2 * (y3 - y1)));
      end;
    (* cas particulier coté p2p3 paralléle à l'axe des abscisses *)
    if (y3 - y2) = 0
    then
      begin
	(* on prendre la droite p1p3 *)
	ap2p3 := (float (-(x3 - x1))) /. (float (y3 - y1));
	bp2p3 := (float (x3 * x3 - x1 * x1 + y3 * y3 - y1 * y1)) /. (float (2 * (y3 - y1)));
      end;
    (* calcul des coordonnes du centre du cercle et son rayon *)
    xC := (!bp1p2 -. !bp2p3) /. (!ap2p3 -. !ap1p2);
    yC := (!ap1p2 *. !xC +. !bp1p2);
    rayon := sqrt ((!xC -. (float x2))*.(!xC -. (float x2)) +. (!yC -. (float y2))*.(!yC -. (float y2)));
    (* cas particulier d'un triangle plat *)
    if (((!ap2p3 -. !ap1p2) <= 0.001 && (!ap2p3 -. !ap1p2) >= 0.) || ((!ap2p3 -. !ap1p2)>= (-0.001) && (!ap2p3 -. !ap1p2) <= 0.))
    then
      begin
	let xmin,ymin = minimum x1 x2 x3 y1 y2 y3 in
	let xmax,ymax = maximum x1 x2 x3 y1 y2 y3 in
	let () = Printf.printf "min %d %d max %d %d" xmin ymin xmax ymax in
	begin
	  xC :=  ((float xmin) +. (float xmax))/.2.;
	  yC :=  ((float ymin) +. (float ymax))/.2.;
	  rayon := sqrt ((!xC +. (float xmin))*.(!xC -. (float xmin)) +. (!yC -. (float ymin))*.(!yC -. (float ymin)));
	end;
      end;
    (* cas où le point1 serait le centre du cercle circonscrit *)
    if (!xC = (float x2) && !yC = (float y2) && not(((!ap2p3 -. !ap1p2) <= 0.001 && (!ap2p3 -. !ap1p2) >= 0.) || ((!ap2p3 -. !ap1p2)>= (-0.001) && (!ap2p3 -. !ap1p2) <= 0.)))
    then
      rayon := sqrt ((!xC -. (float x3))*.(!xC -. (float x3)) +. (!yC -. (float y3))*.(!yC -. (float y3)));
  end;
  (!xC),(!yC), (!rayon) ;;

(* retourne True si le point sommet appartient au cercle circonscrit au triangle *)
let dans_cercle = fun triangle sommet ->
  let xC,yC,rayon = cercle_circonscrit triangle in
  let x = sommet.Map.x in
  let y = sommet.Map.y in
  let distance = sqrt (((xC -. (float x)) *. (xC -. (float x))) +. ((yC -. (float y)) *. (yC -. (float y)))) in
  distance < rayon;;

(* renvoie la liste des arêtes des triangles appartenant au polygone entourant le nouveau point *)
let edge = fun point1 point2 triangle list polygon ->
  let compt = ref 0 in
  (* condition = vrai alors il n'y a pas d'arete partagee *)
  let condition = ref true in
  let rec loop = fun listt ->
    compt := 0;
    match listt with
	[] -> ()
      |tri::queue -> compt := 0;
	(* pour chaque triangle ou regarde s'il a 1 arête en commun avec le segment point1point2 *)
	begin
          match point1 with
            | a when a = tri.p1 ->  compt := (!compt) + 1;
            | b when b = tri.p2 ->  compt := (!compt) + 1;
            | c when c = tri.p3 ->  compt := (!compt) + 1;
	    | inutile -> compt := (!compt);
        end;
        begin
          match point2 with
            | a when a = tri.p1 ->  compt := (!compt) + 1;
            | b when b = tri.p2 ->  compt := (!compt) + 1;
            | c when c = tri.p3 ->  compt := (!compt) + 1;
	    | inutile -> compt := (!compt); 
        end;
	(*  si le compteur vaut 2 et que ce n'est pas l'arete du triangle que l'on considére au départ, alors la condition devient fausse *)
	if ((!compt) = 2 && (tri != triangle))
	then
	  condition := false;
	loop queue;
  in loop list;
(* si la condition est vrai : on ajoute le segment p1p2 au polygone *)
  if (!condition)
  then
      polygon := (point1,point2)::(!polygon);
  (!polygon);;

(* regarde si le triangle appartient à la liste des triangles, si c'est le cas, le triangle est enlevé et on retourne la nouvelle liste de triangle *)
let removetrian = fun tria listtriangle ->
  let listefin = ref [] in
  let rec removetri = fun tri list ->
    match list with
	[] -> ()
      |triangle::queue ->
	if (triangle.p1 = tri.p1 && triangle.p2 = tri.p2 && triangle.p3 = tri.p3 ) ||
	  (triangle.p1 = tri.p1 && triangle.p2 = tri.p3 && triangle.p3 = tri.p2 ) ||
	  (triangle.p1 = tri.p2 && triangle.p2 = tri.p1 && triangle.p3 = tri.p3 ) ||
	  (triangle.p1 = tri.p2 && triangle.p2 = tri.p3 && triangle.p3 = tri.p1 ) ||
	  (triangle.p1 = tri.p3 && triangle.p2 = tri.p1 && triangle.p3 = tri.p2 ) ||
	  (triangle.p1 = tri.p3 && triangle.p2 = tri.p2 && triangle.p3 = tri.p1 )
	then
	  ()
	else listefin := triangle::(!listefin);
	removetri tri queue
  in removetri tria listtriangle;
  (!listefin);;

(* retire toute les triangles qui ont un point commun avec le superTriangle (ce triangle sert juste à établir une base pour la triangulation) *)
let removePointCommunSuperTri = fun triangle supertri trianglelist ->
  begin
    match triangle.p1 with
      | a when a = supertri.p1 -> trianglelist := removetrian triangle (!trianglelist)
      | a when a = supertri.p2 -> trianglelist := removetrian triangle (!trianglelist)
      | a when a = supertri.p3 -> trianglelist := removetrian triangle (!trianglelist)
      | inutile -> trianglelist := (!trianglelist)
  end;
  begin
    match triangle.p2 with
      | a when a = supertri.p1 -> trianglelist := removetrian triangle (!trianglelist)
      | a when a = supertri.p2 -> trianglelist := removetrian triangle (!trianglelist)
      | a when a = supertri.p3 -> trianglelist := removetrian triangle (!trianglelist)
      | inutile -> trianglelist := (!trianglelist)
  end;
  begin
    match triangle.p3 with
      | a when a = supertri.p1 -> trianglelist := removetrian triangle (!trianglelist)
      | a when a = supertri.p2 -> trianglelist := removetrian triangle (!trianglelist)
      | a when a = supertri.p3 -> trianglelist := removetrian triangle (!trianglelist)
      | inutile -> trianglelist := (!trianglelist)
  end;;

(* calcul de la triangulation de Delaunay depuis une liste de points et retourne une liste de triangle correspondante *)
let delaunay = fun pointlist ->
  (* definition de 2 superTriangles englobant tout les points de la triangulation *)
  let superPoint1 = {Map.x=(-20000);Map.y=(-20000);Map.z=0.} in
  let superPoint2 = {Map.x=20000;Map.y=(-20000);Map.z=0.} in
  let superPoint3 = {Map.x=(-20000);Map.y=20000;Map.z=0.} in
  let superPoint4 = {Map.x=20000;Map.y=20000;Map.z=0.} in
  let supertri1 = {p1 = superPoint1 ; p2 = superPoint2 ; p3 = superPoint3 ;equa=(0.,0.,0.) } in
  let supertri2 = {p1 = superPoint4 ; p2 = superPoint2 ; p3 = superPoint3;equa=(0.,0.,0.) } in
  (* la liste de triangle de la triangulation : triangles corrects *)
  let trianglelist = ref (supertri1::supertri2::[]) in
  (* liste des triangles qu'il faudra modifier car ils sont incorrects après ajout du dernier point *)
  let badTriangle = ref [] in
  (* liste des arêtes des mauvais triangles englobant le point ajouté *) 
  let polygon = ref [] in
  (* a chaque fois on part d'une triangulation correcte et on ajout un nouveau point *)
  let rec bouclePourChaquePoint = fun listp ->
    badTriangle := [];
    match listp with
	[] -> ()
      | point::queue ->
	(* on regarde pour chacuns des triangles de la triangulation si il correspond à la condition de ne pas avoir le nouveau point dans son cercle circonscrit, si ce n'est pas le cas on l'ajout à la liste des mauvais trianges *)
        let rec loopAjoutbadtriangle = fun listt ->
          match listt with
              [] -> ()
            | triangle::reste ->
              if dans_cercle triangle point
              then
		badTriangle := triangle::(!badTriangle);
	      loopAjoutbadtriangle reste
        in loopAjoutbadtriangle (!trianglelist);
        polygon := [];
	(* on calcule le polygone englobant tout les mauvais triangles *)
        let rec loopCorrectionTriangle = fun listmt ->
          match listmt with
              [] -> ()
            | triangle::reste ->
	      polygon := edge triangle.p1 triangle.p2 triangle (!badTriangle) polygon;
              polygon := edge triangle.p2 triangle.p3 triangle (!badTriangle) polygon;
              polygon := edge triangle.p1 triangle.p3 triangle (!badTriangle) polygon;
              loopCorrectionTriangle reste;
        in loopCorrectionTriangle (!badTriangle);
	(* on retire tout les mauvais triangles de la liste de triangulation *)
	let rec retirerTriangle = fun listmt ->
	  match listmt with
	      [] -> ()
	    | triangle::reste ->
	      trianglelist := removetrian triangle (!trianglelist);
	      retirerTriangle reste;
	in retirerTriangle (!badTriangle);
	(* on recrée des triangles à partir du point ajouté et des arêtes du polygone et on les ajoute à la liste de la triangulation *)
        let rec loopedgechangement = fun listpoly ->
          match listpoly with
              []->()
            |edge::reste->
              let point1,point2 = edge in
              let newTri = {p1=point1;p2=point2;p3=point;equa=(0.,0.,0.)} in
              trianglelist := newTri::(!trianglelist);
	      loopedgechangement reste
        in loopedgechangement (!polygon);
	bouclePourChaquePoint queue;
  in bouclePourChaquePoint pointlist;
  (* on retire les triangles qui ont un point commun avec les SuperTriangles *)
  let rec retirerSuperTriangle = fun listt ->
    match listt with
      [] -> ()
      | triangle::queue ->
	removePointCommunSuperTri triangle supertri1 trianglelist;
	removePointCommunSuperTri triangle supertri2 trianglelist;
        retirerSuperTriangle queue; 
  in retirerSuperTriangle (!trianglelist);
  (* on renvoie la triangulation correcte *)
  (!trianglelist);;

(* 
(* quelques essais de triangulation *) 
let point1 = {Map.x=(0);Map.y=(0);Map.z=0.};;
let point2 = {Map.x=(10);Map.y=(0);Map.z=0.};;
let point3 = {Map.x=(5);Map.y=(1);Map.z=0.};;
let point4 = {Map.x=(5);Map.y=(5);Map.z=0.};;
let point5 = {Map.x=(10);Map.y=(10);Map.z=0.};;
let point6 = {Map.x=(3);Map.y=(10);Map.z=0.};;
let point7 = {Map.x=(-2);Map.y=(8);Map.z=0.};;
let point8 = {Map.x=(-800);Map.y=(-800);Map.z=0.};;
let point9 = {Map.x=(-700);Map.y=(-700);Map.z=0.};;
let point10 = {Map.x=(-300);Map.y=(-500);Map.z=0.};;
*)
(*
let point1 = {Map.x= 2000;Map.y= -100;Map.z=0.};;
let point2 = {Map.x= -1000;Map.y= -100;Map.z=0.};;
let point3 = {Map.x= 500;Map.y=1500;Map.z=0.};;
let point4 = {Map.x=0;Map.y= 500;Map.z=0.}
let point5 = {Map.x=500;Map.y=0;Map.z=0.}
let point6 = {Map.x=500;Map.y=1000;Map.z=0.}
let point7 = {Map.x= 1000;Map.y= 200;Map.z=0.} 
let point8 = {Map.x=1200;Map.y= 500;Map.z=0.}
let point9 = {Map.x=(-700);Map.y=(-701);Map.z=0.} 
let point10 = {Map.x=(-300);Map.y=(-500);Map.z=0.} 
 
let listeDesPoints = point1::point2::point3::point4::point5::point6::point7::[];;
let listeTriangle = delaunay listeDesPoints;;
List.iter (fun i -> Printf.printf "triangle p1 %d %d p2 %d %d p3 %d %d\n" i.p1.Map.x i.p1.Map.y i.p2.Map.x i.p2.Map.y i.p3.Map.x i.p3.Map.y) listeTriangle;;
*)


(* calcule de la liste des triangles correspondant à notre aeroport *)
let listeDesPoints = Map.point_xyz_points Map.points_alti;;
let listeTriangle = delaunay listeDesPoints;;

