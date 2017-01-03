open Geo

let () =
	let rec koch (p1 : point)(p2 : point)(n : int) : surface list =
		match n with
		| 0 -> [Line(p1,p2)]
		| _ -> let
				facteur = ((distance p1 p2) /. 3.) and
				unitaire = (unitise(p2 -| p1)) in
				let
					np1 = p1 +| (facteur *| unitaire) and
					np3 = p2 -| (facteur *| unitaire) in
					let
						np2 = ((rotate (np3 -| np1) (pi /. 3.)) +| np1)
						in
						(koch p1 np1 (n-1))@(koch np1 np2 (n-1))@(koch np2 np3 (n-1)) @(koch np3 p2 (n-1))
	in
		let pt1 = (point 0.1 0.3) and
			pt2 = (point 0.5 0.9) and
			pt3	= (point 0.9 0.3) and
			n = 8 in
			Aff.draw "ESSAI" ((koch pt1 pt2 n)@(koch pt2 pt3 n)@(koch pt3 pt1 n));;
