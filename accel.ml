
type avion = {tipe : string;
	      mass_dep : float;
	      mass_arri : float;
	      tireradius : float;
	      maxegtstorque : float;
	      egtspower : float;
	      breakawayresistance : float;
	      rollingresistance : float;
	      aerocoef : float;
	      stepcoef : float}


let nexspeedegts avion mass slope speed =
  let slopetorque = -. mass *. 9.81 *. sin( atan(slope /. 100.)) *. avion.tireradius in
  let restorque = ref 0. in
  let egtstorque = ref 0. in
  if speed < 1. then
     restorque := -.mass *. avion.breakawayresistance *. 10. *. avion.tireradius
  else restorque := -.mass *. avion.rollingresistance *. 10. *. avion.tireradius;
  if speed < 1. then
    egtstorque := avion.maxegtstorque
  else egtstorque := (fun a b -> if a <= b then a else b) avion.maxegtstorque (avion.egtspower /. (speed /. avion.tireradius));
  let aerotorque = avion.aerocoef *. speed *. speed in
  let torque = !egtstorque +. slopetorque +. !restorque +. aerotorque in
  let acc = (fun a b -> if a >= b then a else b) 0. (torque /. avion.tireradius /. mass) in
  speed +. avion.stepcoef *. acc
						    

let nexspeedclassic speed =
  speed +. 0.9;;
