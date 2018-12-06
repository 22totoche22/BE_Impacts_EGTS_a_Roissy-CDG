
module Map = Lfpg_map
module Del = Delaunay

let visu (marks,runways,taxiways,listetriangle)=
  Graphics.open_graph("");
  let (largeur, hauteur) = (1000,800) in
  Graphics.resize_window largeur hauteur;
  let (largeur_max,hauteur_max) = (10000,12000) in
  Graphics.set_color 0x000000;
  Graphics.set_line_width 0;
  
  List.iter (fun mark ->
    match mark.Map.coordinates with
      l::[] -> Graphics.fill_circle ((l.Map.x*largeur/largeur_max)+(largeur/2)) ((l.Map.y*hauteur/hauteur_max)+(hauteur/2)) 2
    |_ -> raise Map.Empty)
    marks;

  List.iter (fun taxiway ->
    match taxiway.Map.taxiway_points with
      debut::suite ->  begin
	Graphics.moveto ((debut.Map.x*largeur/largeur_max)+(largeur/2)) ((debut.Map.y*hauteur/hauteur_max)+(hauteur/2));
	List.iter (fun pt -> Graphics.lineto ((pt.Map.x*largeur/largeur_max)+(largeur/2)) ((pt.Map.y*hauteur/hauteur_max)+(hauteur/2))) suite end
    |_ -> raise Map.Empty )
    taxiways;

  Graphics.set_line_width 5;

  
  List.iter (fun runway ->
    match (runway.Map.runway_points) with
      debut::suite ->  begin
	Graphics.moveto ((debut.Map.x*largeur/largeur_max)+(largeur/2)) ((debut.Map.y*hauteur/hauteur_max)+(hauteur/2));
	List.iter (fun pt -> Graphics.lineto ((pt.Map.x*largeur/largeur_max)+(largeur/2)) ((pt.Map.y*hauteur/hauteur_max)+(hauteur/2))) suite end
  |_ -> raise Map.Empty )
    runways;
  
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
  ) listetriangle;
  let rec loop() = loop() in
     loop();; 

let ()=
  visu (Map.marks,Map.runways,Map.taxiways,Del.listeTriangle);;


  
