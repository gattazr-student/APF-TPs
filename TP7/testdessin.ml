(*
let () =  Aff.draw "ESSAI" [
				Geo.Line( (Geo.point 0.2 0.5) , (Geo.point 0.8 0.5)); 
			    Geo.Line( (Geo.point 0.5 0.2) , (Geo.point 0.5 0.8));
				Geo.Circle( (Geo.point 0.5 0.5), 0.3);
				Geo.Line( (Geo.point 0.2 0.5) , (Geo.point 0.4 0.4));
				Geo.Line( (Geo.point 0.4 0.4) , (Geo.point 0.5 0.2));
				Geo.Line( (Geo.point 0.8 0.5) , (Geo.point 0.7 0.7));
				Geo.Line( (Geo.point 0.7 0.7) , (Geo.point 0.5 0.8));
				];;
*)

let () =  Aff.draw "ESSAI" [
				Geo.Line( (Geo.point 0.5 0.3) , (Geo.point 0.5 0.9));
				Geo.Circle( (Geo.point 0.5 0.5), 0.2);
				Geo.Line( (Geo.point 0.1 0.3) , (Geo.point 0.5 0.9));
				Geo.Line( (Geo.point 0.1 0.3) , (Geo.point 0.9 0.3));
				Geo.Line( (Geo.point 0.5 0.9) , (Geo.point 0.9 0.3));
				];;