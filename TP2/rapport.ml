(* TP2 *)

(* TP réalisé par Rémi GATTAZ et Reatha TITH *)

(* exercice 16 *)
let rec longueur (l: int list):int =
  (match l with
      []->0 (* si liste vide, retourne 0 *)
    | x::l1->1+(longueur l1) (* sinon retourne = 1 + longueur (liste - élément courant) *)
  );;

let rec somme (l : int list):int =
  (match l with
      [] -> 0 (* si list vide retorune 1*)
    | x::l1 -> x + (somme l1) (* sinon retourne courant + somme (liste - élément courant) *)
  );;

let rec tous_positif (l : int list):bool =
  (match l with
      [] -> true; (* vrai si list n'a aucun élement*)
    | x::l1 -> (if  x > 0 then tous_positif(l1) else false)
      (* si courant positif, retourne tous_positif(liste - élément courant), sinon faux*)
  );;

(longueur (1::2::3::[]));;
(longueur ([1;2]));;
(longueur []);;

(somme [1;2;3;4]);;
(somme []);;
(somme [1;-1]);;

(tous_positif [1;2;3;4]);;
(tous_positif [1;2;3;-1]);;
(tous_positif []);;



(* Exercice 17 *)
type contenu = Meuble | Objet | Cadre | Plante;;
type solidite = Fragile | Robuste | Moyen;;
type paquet = Paquet of contenu*solidite*int;;

let p1 = Paquet(Meuble,Fragile,15);;

(* Exercice 18 *)
let rec fragiles (aPaquets : paquet list):int =
  (match aPaquets with
      [] -> 0 (* si liste vide, retourne 0 *)
    | (Paquet ((wC:contenu), (wS:solidite), (wP:int)))::wPaquets ->
      (match wS with
          Fragile -> (1 + (fragiles wPaquets)) (*si fragile, retourne 1+fragiles(liste - élément courant) *)
        |_ -> (fragiles wPaquets) (* si autre, retourne fragiles(liste - élément courant) *)
      )
  );;

let inventaire = [];;
(fragiles inventaire);;
let inventaire = (Paquet(Meuble,Moyen,12))::inventaire;;
(fragiles inventaire);;
let inventaire = (Paquet(Objet, Fragile, 1))::(Paquet(Cadre, Robuste, 10))::inventaire;;
(fragiles inventaire);;


(* Exercice 19 *)
let rec legers (aPaquets : paquet list) (wPoidsMax : int):paquet list =
  (match aPaquets with
      [] -> [] (* si liste vide, retourne 0 *)
    | wPaquet::wPaquets -> (let (Paquet ((wC:contenu), (wS:solidite), (wP:int))) = wPaquet in
      (if wP > wPoidsMax then (*si poids > poidsMax, retourne legers(liste - élément courant) *)
        (legers wPaquets wPoidsMax)
      else
        wPaquet::(legers wPaquets wPoidsMax) (* sinon retourne wCourannt::legers(liste - élément courant)*)
      )
    )
  );;

let inventaire = [(Paquet(Meuble,Moyen,12));(Paquet(Objet, Fragile, 1));(Paquet(Cadre, Robuste, 10))];;
(legers [] 1);;
(legers inventaire 1);;
(legers inventaire 10);;
(legers inventaire 12);;
(legers inventaire 20);;

(* Exercice 20 *)
let rec poids_plante (aPaquets : paquet list):int =
  (match aPaquets with
      [] -> 0 (* si list vide, retourn 0 *)
    | (Paquet( (wC:contenu), (wS:solidite), (wP:int)))::wPaquets ->
      (match wC with
          Plante -> wP + (poids_plante wPaquets)
          (* si plante retourne poids courant + poids_plante(liste - élément courant)*)
        |_ -> (poids_plante wPaquets)
          (* sinon retourn poids_plante *)
      )
  );;

(poids_plante []);;
(poids_plante inventaire);;
let inventaire = Paquet(Plante, Moyen, 23)::inventaire;;
(poids_plante inventaire);;
let inventaire = Paquet(Plante, Fragile, 2)::inventaire;;
(poids_plante inventaire);;

(* Exercice 22 *)
let inventaire =  [Paquet (Plante,Fragile,1);Paquet (Meuble, Moyen, 5);
                  Paquet (Cadre, Fragile, 10);Paquet (Objet, Robuste, 25)];;

let rec inventorie (aPaquets: paquet list) (aPaquet : paquet):paquet list =
  (let (Paquet ((wC1:contenu), (wS1:solidite), (wP1:int))) = aPaquet in
    (match aPaquets with
        [] -> [aPaquet] (* si liste vide, retourne liste avec paquet à insérer *)
      | wPaquet::wPaquets ->
        (let (Paquet ((wC2:contenu), (wS2:solidite), (wP2:int))) = wPaquet in
          (if (wP1 > wP2) then (* si poids paquet à insérer > poids paquet courant *)
            wPaquet::(inventorie wPaquets aPaquet) (* ajoute paquet plus loin dans la liste *)
          else
            aPaquet::aPaquets (* sinon, retourne liste avec paquet à insérer au début *)
          )
        )
    )
  );;


let inventaire = ( inventorie inventaire (Paquet (Fleur,Fragile,0)));;
let inventaire = ( inventorie inventaire (Paquet (Objet,Fragile,4)));;
let inventaire = ( inventorie inventaire (Paquet (Cadre,Moyen,12)));;
let inventaire = ( inventorie inventaire (Paquet (Meuble,Robuste,27)));;

(* Exercice 25 *)
type marque = Alpel| Syno | Massung | Liphisp ;;
type produit = Lecteur | Photos | Cameras | Telephones | Ordinateurs ;;
type appareil = Appareil of marque * produit * int * int;;

(* Exercice 27 *)
let rec ajoute_article (aArticles : appareil list) (aArticle : appareil) : appareil list =
  (match aArticles with
      [] -> [aArticle] (* Si liste vide, retourne liste contenant le nouvel article*)
    | wArticle::wArticles ->
      (let (Appareil ((wMarque1:marque), (wProduit1:produit), (wPrix1:int), (wStock1:int)))=aArticle
      and (Appareil ((wMarque2:marque), (wProduit2:produit), (wPrix2:int), (wStock2:int)))=wArticle in
        (if wMarque1=wMarque2 && wProduit1=wProduit2 && wPrix1=wPrix2 then
          (* Si marque, produit et prix similaire, rajoute stock dans existant*)
          (Appareil (wMarque1, wProduit1, wPrix1, wStock1+wStock2))::wArticles
        else
          (* Sinon appel récusrif de ajoute_article et rajouter article courant au début de la liste *)
          wArticle::(ajoute_article wArticles aArticle)
        )
      )
  );;

let a1 = (Appareil (Alpel, Lecteur, 10, 10));;
let a2 = (Appareil (Syno, Photos, 30, 30));;
let a3 = (Appareil (Alpel, Photos, 50, 50));;
let a4 = (Appareil (Syno, Lecteur, 40, 40));;
let articles = [a1;a2;a3;a4];;

let a5 = (Appareil (Massung, Telephones, 1000,1000));;
(ajoute_article articles a5);;
(ajoute_article articles a1);;
