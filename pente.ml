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

let () =
  let condition = point_dans_triangle point_test triangle_test in
  Printf.printf "%b" condition ;;
