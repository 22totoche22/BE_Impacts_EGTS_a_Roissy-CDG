
module Map = Lfpg_map
module Del = Delaunay

let draw_airport (marks,runways,taxiways,listetriangle) largeur largeur_max hauteur hauteur_max =
  Graphics.clear_graph() ;
  Graphics.set_color 0x000000;
  Graphics.set_line_width 0;
  (* on dessine les points *)
  List.iter (fun mark ->
    match mark.Map.coordinates with
	l::[] -> Graphics.fill_circle ((l.Map.x*largeur/largeur_max)+(largeur/2)) ((l.Map.y*hauteur/hauteur_max)+(hauteur/2)) 2
      |_ -> raise Map.Empty)
      marks;

  (* on dessine les taxiways *)
  Graphics.set_line_width 1;
  List.iter (fun taxiway ->
      match taxiway.Map.taxiway_points with
	debut::suite ->  begin
	  Graphics.moveto ((debut.Map.x*largeur/largeur_max)+(largeur/2)) ((debut.Map.y*hauteur/hauteur_max)+(hauteur/2));
	  List.iter (fun pt -> Graphics.lineto ((pt.Map.x*largeur/largeur_max)+(largeur/2)) ((pt.Map.y*hauteur/hauteur_max)+(hauteur/2))) suite end
      |_ -> raise Map.Empty )
      taxiways;

  (* on dessine les pistes *)
    Graphics.set_line_width 7;
    List.iter (fun runway ->
      match (runway.Map.runway_points) with
	debut::suite ->  begin
	  Graphics.moveto ((debut.Map.x*largeur/largeur_max)+(largeur/2)) ((debut.Map.y*hauteur/hauteur_max)+(hauteur/2));
	  List.iter (fun pt -> Graphics.lineto ((pt.Map.x*largeur/largeur_max)+(largeur/2)) ((pt.Map.y*hauteur/hauteur_max)+(hauteur/2))) suite end
      |_ -> raise Map.Empty )
      runways;

    (* on dessine les triangles *)
    Graphics.set_line_width 0;
    Graphics.set_color 0xFF00FF;
    List.iter (fun triangle ->
      Graphics.moveto ((triangle.Del.p1.Map.x*largeur/largeur_max)+(largeur/2)) ((triangle.Del.p1.Map.y*hauteur/hauteur_max)+(hauteur/2));
      Graphics.lineto ((triangle.Del.p2.Map.x*largeur/largeur_max)+(largeur/2)) ((triangle.Del.p2.Map.y*hauteur/hauteur_max)+(hauteur/2));
      Graphics.lineto ((triangle.Del.p3.Map.x*largeur/largeur_max)+(largeur/2)) ((triangle.Del.p3.Map.y*hauteur/hauteur_max)+(hauteur/2));
      Graphics.lineto ((triangle.Del.p1.Map.x*largeur/largeur_max)+(largeur/2)) ((triangle.Del.p1.Map.y*hauteur/hauteur_max)+(hauteur/2));
      Graphics.set_color 0xFF0000;
      Graphics.fill_circle ((triangle.Del.p1.Map.x*largeur/largeur_max)+(largeur/2)) ((triangle.Del.p1.Map.y*hauteur/hauteur_max)+(hauteur/2)) 2;
      Graphics.fill_circle ((triangle.Del.p2.Map.x*largeur/largeur_max)+(largeur/2)) ((triangle.Del.p2.Map.y*hauteur/hauteur_max)+(hauteur/2)) 2;
      Graphics.fill_circle ((triangle.Del.p3.Map.x*largeur/largeur_max)+(largeur/2)) ((triangle.Del.p3.Map.y*hauteur/hauteur_max)+(hauteur/2)) 2;
      Graphics.set_color 0xFF00FF;
    ) listetriangle;;


let wait milli =
  let sec = milli /. 1000. in
  let tm1 = Unix.gettimeofday () in
  while Unix.gettimeofday () -. tm1 < sec do () done;;


let rec event_loop x y (marks,runways,taxiways,listetriangle) hauteur_max largeur_max = 
    (* resize event *)
    let _ = Graphics.wait_next_event [Graphics.Poll]
    and new_x = Graphics.size_x () and new_y = Graphics.size_y ()
    in 
        if new_x <> x || new_y <> y then 
           draw_airport (marks,runways,taxiways,listetriangle) new_x (new_x * largeur_max / x) new_y ( new_y * hauteur_max / y) ;
    event_loop new_x new_y (marks,runways,taxiways,listetriangle) hauteur_max largeur_max;;




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
  Graphics.moveto (largeur/2) ((3000*hauteur/hauteur_max)+(hauteur/2));
  Graphics.set_line_width 0;
  Graphics.set_color Graphics.black;
  let (h,m,s) = timer time in
  let new_time = Printf.sprintf ("%d : %d : %d") h m s in
  Graphics.draw_string  new_time;;


let move_flights points largeur largeur_max hauteur hauteur_max time= 
  Graphics.remember_mode false;(* on désactive la mémoire graphique *)
  
  List.iter (fun (i,s) ->
     let color = ref Graphics.blue in
    let (x,y) =  (i.Map.x,i.Map.y) in
    if s = "E" then color := Graphics.yellow ;
    draw_circle largeur largeur_max hauteur hauteur_max ((x*largeur/largeur_max)+(largeur/2)) ((y*hauteur/hauteur_max)+(hauteur/2)) !color;
  ) points;
  draw_clock time largeur largeur_max hauteur hauteur_max;
  wait 20.; (* pour voir le dessin on fait une attente, unix.sleep ne marche pas *)
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


  
