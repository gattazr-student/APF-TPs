open Geo

let () =
	let rec peano (p1 : point)(p2 : point)(n : int) : surface list =
		match n with
		| 0 -> [Line(p1,p2)]
		| _ -> let
			facteur = ((distance p1 p2) /. 3.) and
			unitaire = (unitise(p2 -| p1)) in
			let
				np1 = p1 +| (facteur *| unitaire) and
				np4 = p2 -| (facteur *| unitaire) in
				let
					np2 = ((rotate (np4 -| np1) (pi /. 2.)) +| np1) and
					np3 = ((rotate (p2  -| np4) (pi /. 2.)) +| np4) and
					np5 = ((rotate (np4 -| np1) (-.(pi /. 2.))) +| np1) and
					np6 = ((rotate (p2  -| np4) (-.(pi /. 2.))) +| np4) and
					n2 = n-1
					in
					(peano p1 np1 n2)@(peano np1 np4 n2)@(peano np4 p2 n2)@
					(peano np1 np2 n2)@(peano np2 np3 n2)@(peano np3 np4 n2)@
					(peano np1 np5 n2)@(peano np5 np6 n2)@(peano np6 np4 n2)
	in
		let pt1 = (point 0.1 0.5) and
			pt2 = (point 0.9 0.5) and
			n = 1 in
			Aff.draw "ESSAI" ((peano pt1 pt2 n)@(peano pt2 pt1 n));;
