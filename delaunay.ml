module Map = Lfpg_map

type triangle ={p1: Map.point; p2: Map.point; p3: Map.point};;

let cercle_circonscrit = fun triangle ->
  let x1 = triangle.p1.Map.x in
  let y1 = triangle.p1.Map.y in
  let x2 = triangle.p2.Map.x in
  let y2 = triangle.p2.Map.y in
  let x3 = triangle.p3.Map.x in
  let y3 = triangle.p3.Map.y in
  if (y2 - y1) = 0
  then
    let ap1p2 = float (-(x2 - x1)) in
    let bp1p2 = float y1 in
    if (y3 - y2) = 0
    then
      let ap2p3 = float (-(x3-x2)) in
      let bp2p3 = float y2 in
      let xC = (bp1p2 -. bp2p3) /. (ap2p3 -. ap1p2) in
      let yC = (ap1p2 *. xC +. bp1p2) in
      let xc = int_of_float xC in
      let yc = int_of_float yC in
      let rayon = sqrt ((xC -. (float x1))*.(xC -. (float x1)) +. (yC -. (float y1))*.(yC -. (float y1))) in
      let pcentre = {Map.x = xc; Map.y = yc; Map.z = 0.} in
      pcentre, rayon
    else
      let ap2p3 = (float (-(x3 - x2))) /. (float (y3 - y2)) in
      let bp2p3 = (float (x3 * x3 - x2 * x2 + y3 * y3 - y2 * y2)) /. (float (2 * (y3 - y2))) in
      let xC = (bp1p2 -. bp2p3) /. (ap2p3 -. ap1p2) in
      let yC =  (ap1p2 *. xC +. bp1p2) in
      let xc = int_of_float xC in
      let yc = int_of_float yC in
      let rayon = sqrt ((xC -. (float x1))*.(xC -. (float x1)) +. (yC -. (float y1))*.(yC -. (float y1))) in
      let pcentre = {Map.x = xc; Map.y = yc; Map.z = 0.} in
      pcentre, rayon
  else
    let ap1p2 = (float (-(x2 - x1))) /. (float (y2 - y1)) in
    let bp1p2 = (float (x2 * x2 - x1 * x1 + y2 * y2 - y1 * y1)) /. (float (2 * (y2 - y1))) in
    if (y3 - y2) = 0
    then
      let ap2p3 = float (-(x3-x2)) in
      let bp2p3 = float y2 in
      let xC = (bp1p2 -. bp2p3) /. (ap2p3 -. ap1p2) in
      let yC =  (ap1p2 *. xC +. bp1p2) in
      let xc = int_of_float xC in
      let yc = int_of_float yC in
      let rayon = sqrt ((xC -. (float x1))*.(xC -. (float x1)) +. (yC -. (float y1))*.(yC -. (float y1))) in
      let pcentre = {Map.x = xc; Map.y = yc; Map.z = 0.} in
      pcentre, rayon
    else
      let ap2p3 = (float (-( x3 - x2))) /. (float (y3 - y2)) in
      let bp2p3 = (float (x3 * x3 - x2 * x2 + y3 * y3 - y2 * y2)) /. (float (2 * (y3 - y2))) in
      let xC = (bp1p2 -. bp2p3) /. (ap2p3 -. ap1p2) in
      let yC =  (ap1p2 *. xC +. bp1p2) in
      let xc = int_of_float xC in
      let yc = int_of_float yC in
      let rayon = sqrt ((xC -. (float x1)) *. (xC -. (float x1)) +. (yC -. (float y1)) *. (yC -. (float y1))) in
      let pcentre = {Map.x = xc; Map.y = yc; Map.z = 0.} in
      pcentre, rayon;;


let dans_cercle = fun triangle sommet ->
  let centre,rayon = cercle_circonscrit triangle in
  let x = sommet.Map.x in
  let y = sommet.Map.y in
  let xC = centre.Map.x in
  let yC = centre.Map.y in
  let distance = sqrt ( (float (xC - x))**2. +. (float (yC - y))**2.) in
  distance < rayon;;

let edge= fun point1 point2 list polygon ->
  let rec loop = fun listt ->
    match listt with
	[] -> ()
      |tri::queue ->
	let compt = ref 0 in
	begin
          match point1 with
            | a when a = tri.p1 ->  compt := (!compt) + 1;
            | b when b = tri.p2 ->  compt := (!compt) + 1;
            | c when c = tri.p3 ->  compt := (!compt) + 1;
        end;
        begin
          match point2 with
            | a when a = tri.p1 ->  compt := (!compt) + 1;
            | b when b = tri.p2 ->  compt := (!compt) + 1;
            | c when c = tri.p3 ->  compt := (!compt) + 1;
        end;
        if (!compt) != 2
	then polygon := (point1,point2)::(!polygon);
	loop queue
  in loop list;
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

let delaunay = fun pointlist ->
  let superPoint1 = {Map.x=0;y=0;z=0.} in
  let superPoint2 = {Map.x=20000;y=0;z=0.} in
  let superPoint3 = {Map.x=0;y=20000;z=0.} in
  let supertri = {p1 = superPoint1 ; p2 = superPoint2 ; p3 = superPoint3 } in
  let trianglelist = ref (supertri::[]) in
  let badTriangle = ref [] in
  let polygon = ref [] in
  let rec bouclePourChaquePoint = fun listp ->
    badTriangle := [];
    match listp with
	[] -> ()
      | point::queue ->
        let rec loopAjoutbadtriangle = fun listt ->
          match listt with
              [] -> ()
            | triangle::reste ->
              if dans_cercle triangle point
              then badTriangle := triangle::(!badTriangle);
              loopAjoutbadtriangle reste
        in loopAjoutbadtriangle (!trianglelist);
        polygon := [];
        let rec loopCorrectionTriangle = fun listmt ->
          match listmt with
              [] -> ()
            | triangle::reste ->
	      polygon := edge triangle.p1 triangle.p2 reste polygon;
              polygon := edge triangle.p2 triangle.p3 reste polygon;
              polygon := edge triangle.p1 triangle.p3 reste polygon;
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
  in bouclePourChaquePoint pointlist;
  let rec retirerSuperTriangle = fun listt ->
    match listt with
      [] -> ()
      | triangle::queue ->
        begin
          match triangle.p1 with
            | a when a = supertri.p1 -> trianglelist := removetrian triangle listt
            | a when a = supertri.p2 -> trianglelist := removetrian triangle listt
            | a when a = supertri.p3 -> trianglelist := removetrian triangle listt
        end;
        begin
          match triangle.p2 with
            | a when a = supertri.p1 -> trianglelist := removetrian triangle listt
            | a when a = supertri.p2 -> trianglelist := removetrian triangle listt
            | a when a = supertri.p3 -> trianglelist := removetrian triangle listt
        end;
        begin
          match triangle.p3 with
            | a when a = supertri.p1 -> trianglelist := removetrian triangle listt
            | a when a = supertri.p2 -> trianglelist := removetrian triangle listt
            | a when a = supertri.p3 -> trianglelist := removetrian triangle listt
        end;
        retirerSuperTriangle queue;
  in retirerSuperTriangle (!trianglelist);
  (!trianglelist);;



let () =
  let listeDesPoints = Map.point_xyz_points Map.points_alti in
  let listeTriangle = delaunay listeDesPoints in
  ();;
