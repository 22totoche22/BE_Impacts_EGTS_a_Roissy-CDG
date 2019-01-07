
module Map = Lfpg_map
module Del = Delaunay


let x_tographics x distance distance_max =
  (x*distance/distance_max)+(distance/2);;
  

let draw_points color width r point largeur largeur_max hauteur hauteur_max = 
  Graphics.set_color color;
  Graphics.set_line_width width;
  Graphics.fill_circle (x_tographics point.Map.x largeur largeur_max) (x_tographics point.Map.y hauteur hauteur_max) r;;


let draw_lines ptdep suite color width largeur largeur_max hauteur hauteur_max =
  Graphics.set_line_width width;
  Graphics.set_color color;
  Graphics.moveto (x_tographics ptdep.Map.x largeur largeur_max) (x_tographics ptdep.Map.y hauteur hauteur_max);
  List.iter (fun pt -> Graphics.lineto (x_tographics pt.Map.x largeur largeur_max) (x_tographics pt.Map.y hauteur hauteur_max)) suite;;


let draw_triangles width color_tri color_pt triangle largeur largeur_max hauteur hauteur_max =
  Graphics.set_line_width width;
  Graphics.set_color color_tri;
  Graphics.moveto (x_tographics triangle.Del.p1.Map.x largeur largeur_max) (x_tographics triangle.Del.p1.Map.y hauteur hauteur_max);
  Graphics.lineto (x_tographics triangle.Del.p2.Map.x largeur largeur_max) (x_tographics triangle.Del.p2.Map.y hauteur hauteur_max);
  Graphics.lineto (x_tographics triangle.Del.p3.Map.x largeur largeur_max) (x_tographics triangle.Del.p3.Map.y hauteur hauteur_max);
  Graphics.lineto (x_tographics triangle.Del.p1.Map.x largeur largeur_max) (x_tographics triangle.Del.p1.Map.y hauteur hauteur_max);
  draw_points color_pt width 2 triangle.Del.p1 largeur largeur_max hauteur hauteur_max;
  draw_points color_pt width 2 triangle.Del.p2 largeur largeur_max hauteur hauteur_max;
  draw_points color_pt width 2 triangle.Del.p3 largeur largeur_max hauteur hauteur_max;;

      
let draw_airport (marks,runways,taxiways,listetriangle) largeur largeur_max hauteur hauteur_max =
  Graphics.clear_graph() ;
  (* on dessine les points *)
  List.iter (fun mark ->
    match mark.Map.coordinates with
	l::[] -> draw_points Graphics.black 0 2 l largeur largeur_max  hauteur hauteur_max
      |_ -> raise Map.Empty)
      marks;
  (* on dessine les taxiways *)
  List.iter (fun taxiway ->
      match taxiway.Map.taxiway_points with
	debut::suite ->  draw_lines debut suite Graphics.black 1 largeur largeur_max hauteur hauteur_max
      |_ -> raise Map.Empty )
      taxiways;
  (* on dessine les pistes *)
    List.iter (fun runway ->
      match (runway.Map.runway_points) with
	debut::suite ->  draw_lines debut suite Graphics.black 7 largeur largeur_max hauteur hauteur_max
      |_ -> raise Map.Empty )
      runways;
  (* on dessine les triangles *)
    List.iter (fun triangle -> draw_triangles 0 0xFF00FF Graphics.red triangle largeur largeur_max hauteur hauteur_max
    ) listetriangle;;


let wait milli =
  
  let sec = milli /. 1000. in
  let tm1 = Unix.gettimeofday () in
  while Unix.gettimeofday () -. tm1 < sec do () done;;
  (*
    soucis en optimisé
  let commande = "sleep %" ^ (string_of_float milli) in
  Sys.command commande
;;*)

(*
let rec event_loop x y (marks,runways,taxiways,listetriangle) hauteur_max largeur_max = 
    (* resize event *)
    let _ = Graphics.wait_next_event [Graphics.Poll]
    and new_x = Graphics.size_x () and new_y = Graphics.size_y ()
    in 
        if new_x <> x || new_y <> y then 
           draw_airport (marks,runways,taxiways,listetriangle) new_x (new_x * largeur_max / x) new_y ( new_y * hauteur_max / y) ;
    event_loop new_x new_y (marks,runways,taxiways,listetriangle) hauteur_max largeur_max;;
*)

let draw_circle largeur largeur_max hauteur hauteur_max x y color = 
  Graphics.set_line_width 0;
  Graphics.set_color color;
  Graphics.fill_circle x y 3;; (* on n'écrit que sur la fenetre graphique pas dans la memoire graphique *)
   

(*
let move_flight list_flights largeur largeur_max hauteur hauteur_max=
  List.iter (fun flight ->
	List.iter (fun pt -> draw_circle largeur largeur_max hauteur hauteur_max ((pt.Map.x*largeur/largeur_max)+(largeur/2)) ((pt.Map.y*hauteur/hauteur_max)+(hauteur/2))) flight.Map.route) list_flights;;

*)

let timer time =
  let h = time / 3600 in
  let rest = time mod 3600 in
  let min = rest / 60 in
  let s = rest mod 60 in
  (h,min,s);;
    

let draw_clock time largeur largeur_max hauteur hauteur_max =
  Graphics.moveto (largeur/2) (x_tographics 3000 hauteur hauteur_max);
  Graphics.set_line_width 0;
  Graphics.set_color Graphics.black;
  let (h,m,s) = timer time in
  let new_time = Printf.sprintf ("%d : %d : %d") h m s in
  Graphics.draw_string  new_time;;


let move_flights points largeur largeur_max hauteur hauteur_max time vitesse= 
  Graphics.remember_mode false;(* on désactive la mémoire graphique *)
  List.iter (fun (i,s) ->
     let color = ref Graphics.blue in
    let (x,y) =  (i.Map.x,i.Map.y) in
    if s = "E" then color := Graphics.yellow ;
    draw_circle largeur largeur_max hauteur hauteur_max (x_tographics x largeur largeur_max) (x_tographics y hauteur hauteur_max) !color;
  ) points;
  draw_clock time largeur largeur_max hauteur hauteur_max;
  wait vitesse; (* pour voir le dessin on fait une attente, unix.sleep ne marche pas *)
  Graphics.remember_mode true; (* on réactive la mémoire *)
  Graphics.synchronize ();; (* on synchronise memoire et fenetre, ce qu'on a dessiné disparait *)




(*
let visu (marks,runways,taxiways,listetriangle)=
  Graphics.open_graph("");
  let (largeur, hauteur) = (1200,800) in
  Graphics.resize_window largeur hauteur;
  let (largeur_max,hauteur_max) = (10000,8000) in
  try draw_airport (marks,runways,taxiways,listetriangle) largeur largeur_max hauteur hauteur_max;
  with Graphics.Graphic_failure _ -> print_endline "Exiting..." ;; 

*)


  
