

type point = {x : int;
	      y : int;
	      z : float}

type mark = {mark_id: string;
	     tipe: int;
	     coordinates: point list}
  
type taxiway = {taxiway_id : string;
		max_speed : int;
		taxiway_category : string;
		one_way : string;
		taxiway_points : point list}

type runway = {runway_marks : string list;
	       runway_points : point list;
	       runway_id : string;
	       left : string;
	       right : string}

type plane = {tipe : string;
	      mass_dep : float;
	      mass_arri : float;
	      tireradius : float;
	      maxegtstorque : float;
	      egtspower : float;
	      breakawayresistance : float;
	      rollingresistance : float;
	      aerocoef : float;
	      stepcoef : float}
  
type flight = {dep_arr : string;
	       flight_id : string;
	       mutable flight_category : plane;
	       mutable flight_stand : string;
	       flight_qfu : string;
	       h_dep : int;
	       h_arr : int;
	       masse : float;
	       mutable route : point list}


let a320 = {tipe = "a320";
 	mass_dep = 69000.;
 	mass_arri = 62000.;
 	tireradius = 0.56;
	maxegtstorque = 16000.;
 	egtspower = 46000.;
 	breakawayresistance = 0.01;
	rollingresistance = 0.007;
	aerocoef = 1.032;
	stepcoef = 4.1};;

let a319 = {tipe = "a319";
 	mass_dep = 63000.;
 	mass_arri = 57000.;
 	tireradius = 0.56;
	maxegtstorque = 16000.;
 	egtspower = 46000.;
 	breakawayresistance = 0.01;
	rollingresistance = 0.007;
	aerocoef = 1.032;
	stepcoef = 4.1};;

let a321 = {tipe = "a321";
 	mass_dep = 81000.;
 	mass_arri = 73000.;
 	tireradius = 0.56;
	maxegtstorque = 16000.;
 	egtspower = 46000.;
 	breakawayresistance = 0.01;
	rollingresistance = 0.007;
	aerocoef = 1.032;
	stepcoef = 4.1};;

exception Empty

  (* --------------------------------------------------------------- *)

  
let stringtotuple s =
  let l =  Str.split (Str.regexp "[,]") s in
  match l with
    [h;t] -> {x = int_of_string h; y = int_of_string t; z = 0.}
  |_ ->raise Empty;;


let read_file_map nom_fichier_map =
  let file = open_in nom_fichier_map in                                 (* ouvre le fichier de descriptio nde l'aeroport *)
  let _marks = [] in
  let _runways = [] in
  let _taxiways = [] in
  let rec read_file_rec marks runways taxiways =
    try
      let line = Str.split (Str.regexp "[ ]") (input_line file) in      (* cree une liste pour la ligne avec le séparateur " " *)
      let first_element = List.hd line in
      if first_element = "P" then
	match line with
	  tipe::id::altitude::coordonnees ->
	    let new_marks = {mark_id = id;
			     tipe  = int_of_string altitude;
			     coordinates = (List.map stringtotuple coordonnees)}::marks in
	    read_file_rec new_marks runways taxiways;
	|_ -> raise Empty 
      else
	if first_element = "L" then
	  match line with
	    tipe::id::vitesse::categorie::route::xyz ->
	      let new_taxiways = {taxiway_id = id;
				  max_speed = int_of_string(vitesse);
				  taxiway_category = categorie;
				  one_way = route;
				  taxiway_points = List.map stringtotuple xyz}::taxiways in
	      read_file_rec marks runways new_taxiways
	  |_ -> raise Empty
	else
	  if first_element = "R" then
	    match line with
	      tipe::id::bout1::bout2::points::xyz ->
		let new_runways = {runway_marks = Str.split (Str.regexp "[,]") points;
				   runway_points =  List.map stringtotuple xyz;
				   runway_id = id;
				   left = bout1;
				   right = bout2}::runways in
		read_file_rec marks new_runways taxiways
	    |_ -> raise Empty
	  else
	    read_file_rec marks runways taxiways
    with End_of_file -> begin close_in file; (marks, runways, taxiways) end in
  read_file_rec _marks _runways _taxiways ;;


let read_file_altitude nom_fichier_altitude =
  let file = open_in nom_fichier_altitude in
  let _points = [] in
  let rec read_file_rec points =
    try
      let line = Str.split (Str.regexp "[ ]") (input_line file) in      (* cree une liste pour la ligne avec le séparateur " " *)
      match line with
	tipe::id::x::y::z::[] ->
	  let new_points = {mark_id = id;
			    tipe  = 0;
			    coordinates = {x = int_of_string x; y = int_of_string y; z = float_of_string z}::[]}::points in
	  read_file_rec new_points;
      |_ -> raise Empty
    with End_of_file -> begin close_in file; points end in
  read_file_rec _points ;;


let read_file_flights nom_fichier_flights =
  let file = open_in nom_fichier_flights in
  let _flights = [] in
  let rec read_file_rec flights =
    try
      let line = Str.split (Str.regexp "[ ]") (input_line file) in      (* cree une liste pour la ligne avec le séparateur " " *)

      match line with
	dep::id::category::stand::runway::prove::desti::_::coordonnees ->
	  let avion = ref a319 in
	  let masse = ref 0. in
	  begin
	    match category with
	      cat when cat = "L" ->  avion:= a319
	    |cat when cat = "M" ->  avion:= a320
	    |cat when cat = "H" ->  avion:= a321
	    |_ -> failwith "category" ;
	  end;
	  begin
	    match dep with
	      dep_ar when dep_ar = "DEP" -> masse := (!avion).mass_dep 
	    |dep_ar when dep_ar = "ARR" -> masse  := (!avion).mass_arri 
	    |_ -> failwith "depart_arrive" ;
	  end;
	  let new_flights = {dep_arr = dep;
			     flight_id  = id;
			     flight_category = !avion;
			     flight_qfu = runway;
			     flight_stand = stand;
			     h_dep = int_of_string prove;
			     h_arr = int_of_string desti;
			     masse = !masse;
			     route = (List.map stringtotuple coordonnees)}::flights in
	  read_file_rec new_flights;
      |_ -> raise Empty
    with End_of_file -> begin close_in file; flights end in
  read_file_rec _flights ;;

let point_xyz_mark mark = 
  mark.coordinates;;

let point_xyz_points points =
  List.map (fun i ->
    match i.coordinates with
      l::[] -> l
    |_ -> raise Empty) points;;

let point_xyz_taxiway taxiway =
  taxiway.taxiway_points;;

let point_xyz_runway runway =
  runway.runway_points;;
  (*

    let print_points listofpoint =
    List.iter (fun a ->
    begin print_string a.mark_id;
    print_int a.tipe;
    (fun l -> List.iter (fun (c,d,e) -> print_int c; print_int d; print_float e ) l ) a.coordinates;
    print_string "\n" end) listofpoint;; 

    let print_runways listofrunway =
    List.iter (fun a ->
    begin print_string a.runway_id;
    print_string a.left;
    print_string a.right;
    (fun l -> List.iter print_string l ) a.runway_marks;
    (fun l -> List.iter (fun (c,d,e) -> print_int c; print_int d; print_float e ) l ) a.runway_points;
    print_string "\n" end) listofrunway;;

    let print_taxiways listoftaxiway =
    List.iter (fun a ->
    begin print_string a.taxiway_id;
    print_int a.max_speed;
    print_string a.taxiway_category;
    print_string a.one_way;
    (fun l -> List.iter (fun (c,d,e) -> print_int c; print_int d; print_float e ) l ) a.taxiway_points;
    print_string "\n" end) listoftaxiway;;
  *)


let (marks,runways,taxiways) = read_file_map "lfpg_map.txt";;
let points_alti = read_file_altitude "lfpg_alti.txt" ;;
let flights = read_file_flights "lfpg_flights_save.txt" ;;

