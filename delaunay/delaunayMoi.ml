(* SUREMENT MODIFIER LES TYPES POUR REPRENDRE LES TIENS *)
type point = {x: int; y: int; z: float};;

type triangle ={p1: point; p2: point; p3: point};;

let cercle_circonscrit = fun triangle ->
  let x1 = triangle.p1.x in
  let y1 = triangle.p1.y in
  let x2 = triangle.p2.x in
  let y2 = triangle.p2.y in
  let x3 = triangle.p3.x in
  let y3 = triangle.p3.y in
  let ap1p2 = ref 0. in
  let ap2p3 = ref 0. in
  let bp1p2 = ref 0. in
  let bp2p3 = ref 0. in
  let xC = ref 0. in
  let yC = ref 0. in
  let rayon = ref 0. in
  let pcentre = ref ({x = 0; y = 0; z = 0.}) in
  begin
    (* cas général *)
    ap1p2 := (float (-(x2 - x1))) /. (float (y2 - y1));
    bp1p2 := (float (x2 * x2 - x1 * x1 + y2 * y2 - y1 * y1)) /. (float (2 * (y2 - y1)));
    ap2p3 := (float (-(x3 - x2))) /. (float (y3 - y2));
    bp2p3 := (float (x3 * x3 - x2 * x2 + y3 * y3 - y2 * y2)) /. (float (2 * (y3 - y2)));
    (* cas particulier *)
    if (y2 - y1) = 0
    then
      begin
	ap1p2 := (float (-(x2 - x1)));
	bp1p2 := (float y1);
      end;
    if (y3 - y2) = 0
    then
      begin
	ap2p3 := (float (-(x3 - x2)));
	bp2p3 := (float y2);
      end;
    xC := (!bp1p2 -. !bp2p3) /. (!ap2p3 -. !ap1p2);
    yC := (!ap1p2 *. !xC +. !bp1p2);
    rayon := sqrt ((!xC -. (float x1))*.(!xC -. (float x1)) +. (!yC -. (float y1))*.(!yC -. (float y1)));
    pcentre := {x = (int_of_float !xC); y = (int_of_float !yC); z = 0.};
    if !pcentre = triangle.p1
    then
      rayon := sqrt ((!xC -. (float x2))*.(!xC -. (float x2)) +. (!yC -. (float y2))*.(!yC -. (float y2)));
  end;
  (!pcentre), (!rayon) ;;

let dans_cercle = fun triangle sommet ->
  let centre,rayon = cercle_circonscrit triangle in
  let () = Printf.printf " danscercle %d %d %f\n" centre.x centre.y rayon in
  let x = sommet.x in
  let y = sommet.y in
  let xC = centre.x in
  let yC = centre.y in
  let distance = sqrt ( (float (xC - x))**2. +. (float (yC - y))**2.) in
  distance < rayon;;

let edge = fun point1 point2 list polygon ->
  let compt = ref 0 in
  (* condition = vrai alors il n'y a pas d'arete partagee *)
  let condition = ref true in
  let rec loop = fun listt ->
    compt := 0;
    match listt with
	[] -> ()
      |tri::queue ->
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
	Printf.printf "compteur %d\n" (!compt);
	if (!compt) = 2
	then
	  condition := false;
	loop queue;
  in loop list;
  if (!condition)
  then
    begin
      polygon := (point1,point2)::(!polygon);
      Printf.printf "edge ajout p1 %d %d p2 %d %d\n" point1.x point1.y point2.x point2.y;
    end
  else
    Printf.printf "pas de cote partage\n";
  (!polygon);;

let removetrian = fun tria listtriangle ->
  let listefin = ref [] in
  let rec removetri = fun tri list ->
    match list with
	[] -> ()
      |triangle::queue ->
	if triangle = tri
	then listefin := (List.append (!listefin) queue)
	else listefin := triangle::(!listefin);
	removetri tri queue
  in removetri tria listtriangle;
  (!listefin);;

let removePointCommunSuperTri = fun triangle supertri trianglelist listt ->
  begin
    match triangle.p1 with
      | a when a = supertri.p1 -> trianglelist := removetrian triangle listt
      | a when a = supertri.p2 -> trianglelist := removetrian triangle listt
      | a when a = supertri.p3 -> trianglelist := removetrian triangle listt
      | inutile -> trianglelist := (!trianglelist)
  end;
  begin
    match triangle.p2 with
      | a when a = supertri.p1 -> trianglelist := removetrian triangle listt
      | a when a = supertri.p2 -> trianglelist := removetrian triangle listt
      | a when a = supertri.p3 -> trianglelist := removetrian triangle listt
      | inutile -> trianglelist := (!trianglelist)
  end;
  begin
    match triangle.p3 with
      | a when a = supertri.p1 -> trianglelist := removetrian triangle listt
      | a when a = supertri.p2 -> trianglelist := removetrian triangle listt
      | a when a = supertri.p3 -> trianglelist := removetrian triangle listt
      | inutile -> trianglelist := (!trianglelist)
  end;;
  
let delaunay = fun pointlist ->
  let superPoint1 = {x=0;y=0;z=0.} in
  let superPoint2 = {x=20000;y=0;z=0.} in
  let superPoint3 = {x=0;y=20000;z=0.} in
  let superPoint4 = {x=20000;y=20000;z=0.} in
  let supertri1 = {p1 = superPoint1 ; p2 = superPoint2 ; p3 = superPoint3 } in
  let supertri2 = {p1 = superPoint4 ; p2 = superPoint2 ; p3 = superPoint3 } in
  let trianglelist = ref (supertri1::supertri2::[]) in
  let badTriangle = ref [] in
  let polygon = ref [] in
  let rec bouclePourChaquePoint = fun listp ->
    badTriangle := [];
    match listp with
	[] -> ()
      | point::queue ->
	let () = Printf.printf "\nPoint : %d %d\n" point.x point.y in
        let rec loopAjoutbadtriangle = fun listt ->
          match listt with
              [] -> ()
            | triangle::reste ->
              if dans_cercle triangle point
              then
		begin
		  Printf.printf "mauvais triangle %d %d %d %d %d %d\n" triangle.p1.x triangle.p1.y triangle.p2.x triangle.p2.y triangle.p3.x triangle.p3.y;
		  badTriangle := triangle::(!badTriangle);
		end
	      else
		Printf.printf "false pas un mauvais triangle\n";
	      loopAjoutbadtriangle reste
        in loopAjoutbadtriangle (!trianglelist);
        polygon := [];
        let rec loopCorrectionTriangle = fun listmt ->
          match listmt with
              [] -> ()
            | triangle::reste ->
	      Printf.printf "Mauvais triangle boucle correc %d %d %d %d %d %d\n" triangle.p1.x triangle.p1.y triangle.p2.x triangle.p2.y triangle.p3.x triangle.p3.y;
	      polygon := edge triangle.p1 triangle.p2 listmt polygon;
              polygon := edge triangle.p2 triangle.p3 listmt polygon;
              polygon := edge triangle.p1 triangle.p3 listmt polygon;
              trianglelist := removetrian triangle (!trianglelist);
              loopCorrectionTriangle reste;
        in loopCorrectionTriangle (!badTriangle);
        let rec loopedgechangement = fun listpoly ->
          match listpoly with
              []->()
            |edge::reste->
              let point1,point2 = edge in
              let newTri = {p1=point1;p2=point2;p3=point} in
              trianglelist := newTri::(!trianglelist);
	      loopedgechangement reste
        in loopedgechangement (!polygon);
	bouclePourChaquePoint queue;
  in bouclePourChaquePoint pointlist;
 (* let rec retirerSuperTriangle = fun listt ->
    match listt with
      [] -> ()
      | triangle::queue ->
	removePointCommunSuperTri triangle supertri1 trianglelist listt;
	removePointCommunSuperTri triangle supertri2 trianglelist listt;
        retirerSuperTriangle queue; 
  in retirerSuperTriangle (!trianglelist);*)
  (!trianglelist);;



let () =
  let point1 = {x=2;y=5;z=0.} in
  let point2 = {x=10;y=15;z=0.} in
  let point3 = {x=30;y=50;z=0.} in
  let point4 = {x=20;y=10;z=0.} in
  let point5 = {x=15;y=5;z=0.} in
  let point6 = {x=3;y=3;z=0.} in
  let listeDesPoints = point1::point2::point3::point4::point5::point6::[] in
  let () = Printf.printf "longueur point %d \n" (List.length listeDesPoints) in
  let listeTriangle = delaunay listeDesPoints in
  let () = Printf.printf "longueur triangle %d \n" (List.length listeTriangle) in
  match listeTriangle with
      [] -> Printf.printf "erreur"
    | t1::rest -> Printf.printf "t1 = %d %d %d %d %d %d\n" (t1.p1.x) (t1.p1.y) t1.p2.x t1.p2.y t1.p3.x t1.p3.y ;;
