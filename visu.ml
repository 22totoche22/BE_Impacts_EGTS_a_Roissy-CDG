  
let visu (marks,runways,taxiways)=
  Graphics.open_graph("");
  let (largeur, hauteur) = (1000,800) in
  Graphics.resize_window largeur hauteur;
  let (largeur_max,hauteur_max) = (10000,8000) in
  Graphics.set_color 0x000000;
  Graphics.set_line_width 0;
  
  List.iter (fun mark ->
    match (Lfpg_map.point_xyz_mark mark) with
      (x,y,z)::[] ->  Graphics.fill_circle ((x*largeur/largeur_max)+(largeur/2)) ((y*hauteur/hauteur_max)+(hauteur/2)) 2
  |_ -> raise Lfpg_map.Empty )
    marks;

  List.iter (fun taxiway ->
    match (Lfpg_map.point_xyz_taxiway taxiway) with
      (x,y,z)::suite ->  begin
	Graphics.moveto ((x*largeur/largeur_max)+(largeur/2)) ((y*hauteur/hauteur_max)+(hauteur/2));
	List.iter (fun (a,b,c) -> Graphics.lineto ((a*largeur/largeur_max)+(largeur/2)) ((b*hauteur/hauteur_max)+(hauteur/2))) suite end
  |_ -> raise Lfpg_map.Empty )
    taxiways;
  Graphics.set_line_width 5;
  List.iter (fun runway ->
    match (Lfpg_map.point_xyz_runway runway) with
      (x,y,z)::suite ->  begin
	Graphics.moveto ((x*largeur/largeur_max)+(largeur/2)) ((y*hauteur/hauteur_max)+(hauteur/2));
	List.iter (fun (a,b,c) -> Graphics.lineto ((a*largeur/largeur_max)+(largeur/2)) ((b*hauteur/hauteur_max)+(hauteur/2))) suite end
  |_ -> raise Lfpg_map.Empty )
    runways;
  let rec loop() = loop() in
     loop();; 

let ()=
  visu (Lfpg_map.marks,Lfpg_map.runways,Lfpg_map.taxiways);;


  
