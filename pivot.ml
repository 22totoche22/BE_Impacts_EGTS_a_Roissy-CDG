module Del = Delaunay
module Map = Lfpg_map
  
type matrice = float array array

(*
let triangle1 = {Del.p1 = {Map.x = 1; Map.y=0 ; Map.z=0. };
		 Del.p2 = {Map.x = 0; Map.y= 1; Map.z= 0.};
		 Del.p3 = {Map.x = 0; Map.y=0 ; Map.z=1. };
		 Del.equa = (0.,0.,0.,0.)
		} ;;
let triangle2 = {Del.p1 = {Map.x =5 ; Map.y=8 ; Map.z=2. };
		 Del.p2 = {Map.x =6 ; Map.y=9 ; Map.z=5. };
		 Del.p3 = {Map.x =7 ; Map.y=10 ; Map.z=4. };
		 Del.equa = (0.,0.,0.,0.);
		} ;;
let trianglelist = triangle1::triangle2::[];;
*)
 
let triangle_to_matrix triangle =
  let a1 = float triangle.Del.p1.Map.x in
  let b1 = float triangle.Del.p1.Map.y in
  let c1 = triangle.Del.p1.Map.z in
  let a2 = float triangle.Del.p2.Map.x in
  let b2 = float triangle.Del.p2.Map.y in
  let c2 = triangle.Del.p2.Map.z in
  let a3 = float triangle.Del.p3.Map.x in
  let b3 = float triangle.Del.p3.Map.y in
  let c3 = triangle.Del.p3.Map.z in
  let matrice_A = [|
    [|a1 ; b1 ;1.|];
      [|a2; b2; 1.|];
      [|a3; b3; 1.|]
		  |] in
  let vecteur_B = [|c1;c2;c3|] in
  (matrice_A,vecteur_B);;

let trianglelistwithequa triangle_list =
  
  List.iter (fun i ->
    let (matrice,vecteur) = triangle_to_matrix i in
    let sol = Solve.solve matrice vecteur in
    let solu_a= sol.(0) in
    let solu_b = sol.(1) in
    let solu_c = sol.(2) in
    let solu = (solu_a, solu_b,solu_c) in 
    i.Del.equa <- solu
  ) triangle_list;;

let print_triangle triangle =
  begin
    print_string " point1 : ";
    print_int triangle.Del.p1.Map.x;
    print_string ",";
    print_int triangle.Del.p1.Map.y;
    print_string ",";
    print_float triangle.Del.p1.Map.z;
    print_string " point2 : ";
    print_int triangle.Del.p2.Map.x;
    print_string ",";
    print_int triangle.Del.p2.Map.y;
    print_string ",";
    print_float triangle.Del.p2.Map.z;
    print_string " point3 : ";
    print_int triangle.Del.p3.Map.x;
    print_string ",";
    print_int triangle.Del.p3.Map.y;
    print_string ",";
    print_float triangle.Del.p3.Map.z;
    print_string "\n equation ";
    let (a,b,c) = triangle.Del.equa in
    print_string "z = ";print_float a; print_string "x +" ; print_float b; print_string "y+" ;print_float c;
    let valu = a *. (float triangle.Del.p1.Map.x) +. b *. (float triangle.Del.p1.Map.y) +. c in
    print_string "\n";
    print_float valu;
    print_string "\n"
    
  end;;

(*
let ()=
  trianglelistwithequa Del.listeTriangle;
    List.iter (fun i -> print_triangle i ) Del.listeTriangle;;
*)
    
