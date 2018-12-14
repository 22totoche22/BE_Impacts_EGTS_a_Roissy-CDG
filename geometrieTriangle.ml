module Map = Lfpg_map
module Del = Delaunay


let produit_vectoriel (abx,aby) (bcx,bcy) =
  abx * bcy - aby * bcx;;

let point_test =  {Map.x = 2; Map.y = 1 ; Map.z =0. }
let triangle_test = {Del.p1 = {Map.x = 1; Map.y=1 ; Map.z=0. };
		 Del.p2 = {Map.x = 2; Map.y= 5; Map.z= 0.};
		 Del.p3 = {Map.x = 5; Map.y=1 ; Map.z=1. };
		 Del.equa = (0.,0.,0.)
		} 
  
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

(* retourne la liste des triangles auquels appartient le point *)
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



(* il faut une fonction pour recuperer les coeffs d'une droite
let coeffDroite = fun point1 point2 -> 
 attention cas x1 = x2 !*)

(* fonction intersection entre un segment et un cercle *)
let intersecSegCercle = fun point1 point2 distance ->
  let x3 = ref 0. in
  let y3 = ref 0. in
  let z3 = ref 0. in
  begin
    if (point2.Map.x = point1.Map.x)
    then
      if (point2.Map.y >= point1.Map.y)
      then
	begin
	  x3 := float point1.Map.x;
	  y3 := distance +. (float  point1.Map.y);
	  z3 := (point2.Map.z -. point1.Map.z) /. (float (point2.Map.y - point1.Map.y)) *. (!y3 -. (float point1.Map.y)) +. point1.Map.z; 
	end
      else
	begin
	  x3 := float point1.Map.x;
	  y3 := (float  point1.Map.y) -. distance;
	  z3 := (point2.Map.z -. point1.Map.z) /. (float (point2.Map.y - point1.Map.y)) *. (!y3 -. (float point1.Map.y)) +. point1.Map.z; 
	end
    else
      let coeffdroite = (float (point2.Map.y - point1.Map.y)) /. (float (point2.Map.x - point1.Map.x)) in
      begin
	if (point2.Map.x > point1.Map.x)
	then
	  x3 := (float point1.Map.x) +. distance /. (sqrt (1. +. coeffdroite *. coeffdroite))
	else
	  x3 := (float point1.Map.x) -. distance /. (sqrt (1. +. coeffdroite *. coeffdroite));
	if (point2.Map.y >= point1.Map.y)
	then
	  y3 := coeffdroite *. distance /. (sqrt (1. +. coeffdroite *. coeffdroite)) +. (float point1.Map.y)
	else
	  y3 := (float point1.Map.y) -. coeffdroite *. distance /. (sqrt (1. +. coeffdroite *. coeffdroite));
	z3 := (point2.Map.z -. point1.Map.z) /. (float (point2.Map.x - point1.Map.x)) *. (!x3 -. (float point1.Map.x)) +. point1.Map.z; 
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
