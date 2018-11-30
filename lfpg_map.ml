
type coordinates_xy = int*int

type point = {p_id: string;
	      altitude: int;
	      coordinates: coordinates_xy list}
  
type taxiway = {t_id : string;
		speed : int;
		category :string;
		one_way : string;
		t_xyz : coordinates_xy list}

type runway = {r_pts : string list;
	       r_xys : coordinates_xy list;
	       qfu : string;
	       left : string;
	       right : string}
exception Empty
  
let stringtotuple s =
  let l =  Str.split (Str.regexp "[,]") s in
  match l with
    [h;t] -> ((int_of_string h),(int_of_string t))
  |_ ->raise Empty;;


let read_file nom_fichier =
  let file = open_in nom_fichier in
  let points = ref [] in
  let runways = ref [] in
  let taxiways = ref [] in
  let rec read_file_rec () =
    try
      let line =Str.split (Str.regexp "[ ]") (input_line file) in
      if List.hd line = "P" then
	match line with
	  tipe::id::altitude::coordonnees ->
	    let new_points = {p_id = id;
			      altitude = int_of_string altitude;
			      coordinates = (List.map stringtotuple coordonnees)}::(!points) in
	    points:= new_points;
	    read_file_rec ();
	|_ -> raise Empty 
      else
	if List.hd line = "L" then
	  match line with
	    tipe::id::vitesse::categorie::route::xyz ->
	      let new_taxiways = {t_id = id;
				  speed = int_of_string(vitesse);
				  category = categorie;
				  one_way = route;
				  t_xyz = List.map stringtotuple xyz}::(!taxiways) in
	      taxiways:= new_taxiways;
	      read_file_rec()
	  |_ -> raise Empty
	else
	  if List.hd line = "R" then
	    match line with
	      tipe::id::bout1::bout2::points::xyz ->
		let new_runways = {r_pts = Str.split (Str.regexp "[,]") points;
				   r_xys =  List.map stringtotuple xyz;
				   qfu = id;
				   left = bout1;
				   right = bout2}::(!runways) in
		runways:= new_runways;
		read_file_rec()
	    |_ -> raise Empty
	  else
	    read_file_rec();
    with End_of_file -> close_in file
  in read_file_rec ();
  (points,runways,taxiways);;




let print_points listofpoint =
  List.iter (fun a ->
    begin print_string a.p_id;
      print_int a.altitude;
    (fun l -> List.iter (fun (c,d) -> print_int c; print_int d ) l ) a.coordinates;
    print_string "\n" end) listofpoint;;

let print_runways listofrunway =
  List.iter (fun a ->
    begin print_string a.qfu;
      print_string a.left;
      print_string a.right;
       (fun l -> List.iter print_string l ) a.r_pts;
      (fun l -> List.iter (fun (c,d) -> print_int c; print_int d ) l ) a.r_xys;
    print_string "\n" end) listofrunway;;

let print_taxiways listoftaxiway =
  List.iter (fun a ->
    begin print_string a.t_id;
      print_int a.speed;
      print_string a.category;
      print_string a.one_way;
      (fun l -> List.iter (fun (c,d) -> print_int c; print_int d ) l ) a.t_xyz;
    print_string "\n" end) listoftaxiway;;


let () =
  let (points,runways,taxiways) = read_file "lfpg_map.txt" in
  print_points !points;
  print_taxiways !taxiways;
  print_runways !runways;;
    
