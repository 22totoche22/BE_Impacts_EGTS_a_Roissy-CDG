module Del = Delaunay
module Map = Lfpg_map
type matrice = float array array


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

 
let triangle_to_matrix triangle =
  [|
    [|float triangle.Del.p1.Map.x; float triangle.Del.p1.Map.y;triangle.Del.p1.Map.z|];
      [|float triangle.Del.p2.Map.x;float triangle.Del.p2.Map.y;triangle.Del.p2.Map.z|];
      [|float triangle.Del.p3.Map.x;float triangle.Del.p3.Map.y;triangle.Del.p3.Map.z|]
  |];;


let trianglelistwithequa triangle_list =
  
  List.iter (fun i ->
    let matrice = triangle_to_matrix i in
    let sol = Solve.solve matrice [|1.;1.;1.|] in
    let solu_a= sol.(0) in
    let solu_b = sol.(1) in
    let solu_c = sol.(2) in
    let solu = (solu_a, solu_b,solu_c,-.1.) in 
    i.Del.equa <- solu
  ) triangle_list;;

let print_triangle triangle =
  begin
    print_string " point1 : ";
    print_int triangle.Del.p1.Map.x;
    print_int triangle.Del.p1.Map.y;
    print_float triangle.Del.p1.Map.z;
    print_string " point2 : ";
    print_int triangle.Del.p2.Map.x;
    print_int triangle.Del.p2.Map.y;
    print_float triangle.Del.p2.Map.z;
    print_string " point3 : ";
    print_int triangle.Del.p3.Map.x;
    print_int triangle.Del.p3.Map.y;
    print_float triangle.Del.p3.Map.z;
    print_string "\n equation ";
    let (a,b,c,d) = triangle.Del.equa in
    print_float a; print_string "x +" ; print_float b; print_string "y+" ;print_float c;print_string "z +";print_float d;
    print_string " autre\n "
    
  end;;

let ()=
  trianglelistwithequa trianglelist;
    List.iter (fun i -> print_triangle i ) trianglelist;;

    
