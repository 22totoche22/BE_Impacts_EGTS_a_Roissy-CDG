module Map = Lfpg_map
module Del = Delaunay


(* retourne a et b tel que pour la droite passant par les points A et B on a y = a * x + b *)
let equadroite = fun pointA pointB ->
  let a = ref 0. in
  let b = ref 0. in
  (* cas général *)
  if (pointA.Map.x != pointB.Map.x)
  then
    begin
      a :=  (float (pointB.Map.y - pointA.Map.y)) /. (float (pointB.Map.x - pointA.Map.x));
      b := (float pointB.Map.y) -. (float pointB.Map.x) *. !a;
    end
  (* cas particulier d'une droite paralléle à l'axe des ordonnées *)
  else
    begin(* attention a ne servira pas dans ce cas la  *)
      a := infinity;
      b := float pointB.Map.x;
    end;
  !a,!b;;

(* calcul le produit vectoriel des droites ab et bc *)
let produit_vectoriel (abx,aby) (bcx,bcy) =
  abx * bcy - aby * bcx;;

(*
let point_test =  {Map.x = 2; Map.y = 1 ; Map.z =0. }
let triangle_test = {Del.p1 = {Map.x = 1; Map.y=1 ; Map.z=0. };
		 Del.p2 = {Map.x = 2; Map.y= 5; Map.z= 0.};
		 Del.p3 = {Map.x = 5; Map.y=1 ; Map.z=1. };
		 Del.equa = (0.,0.,0.)
		} 
*)
  
 (* return True si le point est dans le triangle *)
let point_dans_triangle point_m triangle_abc =
  let (ax,ay) = (triangle_abc.Del.p1.Map.x,triangle_abc.Del.p1.Map.y) in
  let (bx,by) = (triangle_abc.Del.p2.Map.x,triangle_abc.Del.p2.Map.y) in
  let (cx,cy) = (triangle_abc.Del.p3.Map.x,triangle_abc.Del.p3.Map.y) in
  let (mx,my) = (point_m.Map.x, point_m.Map.y) in
  let vect_ab = (bx - ax, by - ay) in
  let vect_am = (mx - ax, my - ay) in
  let vect_ac = (cx - ax, cy - ay) in
  let vect_ba = (ax - bx, ay - by) in
  let vect_bm = (mx - bx, my - by) in
  let vect_bc = (cx - bx, cy - by) in
  let vect_ca = (ax - cx, ay - cy) in
  let vect_cm = (mx - cx, my - cy) in
  let vect_cb = (bx - cx, by - cy) in
  let ab_ac = (produit_vectoriel  vect_ab vect_am) * (produit_vectoriel  vect_am vect_ac) in
  let ba_bc = (produit_vectoriel  vect_ba vect_bm) * (produit_vectoriel  vect_bm vect_bc) in
  let ca_cb = (produit_vectoriel  vect_ca vect_cm) * (produit_vectoriel  vect_cm vect_cb) in
  (ab_ac >= 0 && ba_bc >= 0 && ca_cb >= 0);;

(* retourne la liste des triangles auquels appartient le point (>1 si c'est un sommet ou une arête *)
let dansQuelTriangle = fun point listeTriangles ->
  let rec loop = fun listetriangle liste ->
    match listetriangle with
	[] -> liste;
      | triangle::queue ->
	  if (point_dans_triangle point triangle)
	  then
	    let newliste = triangle::liste
	    in loop queue newliste
	  else 
	    let newliste = liste in
	    loop queue newliste 
  in loop listeTriangles [] ;;


(* fonction intersection entre un segment et un cercle (de centre p1), retourne le point d'intersection *)
let intersecSegCercle = fun point1 point2 distance ->
  let x3 = ref 0. in
  let y3 = ref 0. in
  let z3 = ref 0. in
  begin
    (* cas alignement du segment avec l'axe des ordonnées *)
    if (point2.Map.x = point1.Map.x)
    then
      (* point dans l'ordre des y croissant : p1 - pintersection - p2 *)
      if (point2.Map.y >= point1.Map.y)
      then
	begin
	  x3 := float point1.Map.x;
	  y3 := distance +. (float  point1.Map.y);
	  z3 := (point2.Map.z -. point1.Map.z) /. (float (point2.Map.y - point1.Map.y)) *. (!y3 -. (float point1.Map.y)) +. point1.Map.z; 
	end
      (* point dans l'ordre des y croissant : p2 - pintersection - p1 *)
      else
	begin
	  x3 := float point1.Map.x;
	  y3 := (float  point1.Map.y) -. distance;
	  z3 := (point2.Map.z -. point1.Map.z) /. (float (point2.Map.y - point1.Map.y)) *. (!y3 -. (float point1.Map.y)) +. point1.Map.z; 
	end
    (* cas général *)
    else
      let coeffdroite = (float (point2.Map.y - point1.Map.y)) /. (float (point2.Map.x - point1.Map.x)) in
      begin
	(* cas alignement des x croissant : p1 - pintersection - p2 *)
	if (point2.Map.x > point1.Map.x)
	then
	  begin
	    x3 := (float point1.Map.x) +. distance /. (sqrt (1. +. coeffdroite *. coeffdroite));
	    y3 := coeffdroite *. distance /. (sqrt (1. +. coeffdroite *. coeffdroite)) +. (float point1.Map.y);
	    z3 := (point2.Map.z -. point1.Map.z) /. (float (point2.Map.x - point1.Map.x)) *. (!x3 -. (float point1.Map.x)) +. point1.Map.z;
	  end
	(* cas alignement des x croissant : p2 - pintersection - p1 *)
	else
	  begin
	    x3 := (float point1.Map.x) -. distance /. (sqrt (1. +. coeffdroite *. coeffdroite));
	    y3 := (float point1.Map.y) -. coeffdroite *. distance /. (sqrt (1. +. coeffdroite *. coeffdroite));
	    z3 := (point2.Map.z -. point1.Map.z) /. (float (point2.Map.x - point1.Map.x)) *. (!x3 -. (float point1.Map.x)) +. point1.Map.z;
	  end;
      end;
  end;
  {Map.x= int_of_float (!x3);Map.y= int_of_float (!y3);Map.z= (!z3)};;

(*
let () =
  let condition = point_dans_triangle point_test triangle_test in
  Printf.printf "%b" condition ;;
*)

(*
let p1 = {Map.x=0;Map.y=0;Map.z=0.}
let p2 = {Map.x=5;Map.y=5;Map.z=10.}
let p3 = {Map.x=5;Map.y=0;Map.z=5.}
let distance = 2.9
let result = intersecSegCercle p1 p2 distance ;;
Printf.printf "\nINTERSECTION point %d %d %f\n" result.Map.x result.Map.y result.Map.z
*)
type rectangle = {p1:Map.point; p2 : Map.point; p3 :Map.point; p4 :Map.point};;

(* retourne true si le point est dans un rectangle *)
let pointdansrectangle point rectangle  =
  let (ax,ay) = (rectangle.p1.Map.x,rectangle.p1.Map.y) in
  let (bx,by) = (rectangle.p2.Map.x,rectangle.p2.Map.y) in
  let (cx,cy) = (rectangle.p3.Map.x,rectangle.p3.Map.y) in
  let (dx,dy) = (rectangle.p4.Map.x,rectangle.p4.Map.y) in
  let (mx,my) = (point.Map.x, point.Map.y) in
  let vect_ab = (bx - ax, by - ay) in
  let vect_am = (mx - ax, my - ay) in
  let vect_ad = (dx - ax, dy - ay) in
  let vect_cd = (dx - cx, dy - cy) in
  let vect_cm = (mx - cx, my - cy) in
  let vect_cb = (bx - cx, by - cy) in
  let ab_ad = (produit_vectoriel  vect_ab vect_am) * (produit_vectoriel  vect_am vect_ad) in
  let cb_cd = (produit_vectoriel  vect_cb vect_cm) * (produit_vectoriel  vect_cm vect_cd) in
  (ab_ad >= 0 && cb_cd >= 0)
  
  
