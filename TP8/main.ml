(*
  fichier: main.ml
  auteur(s): Colas Le Guernic, Sophie Quinton

  Une fois compilé le premier argument du programme définit le type ordonné  à utiliser, le deuxième argument définit l'algorithme de tri et le troisième
  le nom du fichier contenant les éléments à trier.
*)

(*
  On commence par définir des chaînes de caractères qui vont nous
  permettre de construire des messages d'erreur.
*)
let types_autorises="\t\"int\"\n\t\"string\"\n"
and algorithmes_autorises="\t\"bubble\" (tri ?bulles)\n\t\"qs\" (quicksort)\n"

let doc=
  "\nLe premier argument du programme définit le type ordonné à utiliser,
le deuxième argument défiinit l'algorithme de tri et le troisième le nom
du fichier contenant les éléments à trier. Les types autorisés sont:\n"
  ^types_autorises^"Les algorithmes implémentés sont:\n"^algorithmes_autorises

(*
  On vérifie ensuite si on a le bon nombre d'arguments, avant de les mettre
  dans trois variables au nom explicite. S'il y a trop d'arguments, les arguments
  en trop sont ignorés. S'il manque des arguments, on renvoie la documentation
  du programme.

  Sys.argv : string array
  Ligne de commande utilisée pour lancer le programme. Le premier élément
  est le nom de la commande. Les autres éléments sont les arguments donnés
  au programme.
*)

let _=
  if Array.length Sys.argv < 3 then (
    print_endline ("Ce programme prend trois arguments.\n"^doc);
    exit 1
  )

let type_utilise=Sys.argv.(1)
and algo_utilise=Sys.argv.(2)
and file_name=Sys.argv.(3)

(*
On ouvre le fichier passé en argument du programme.
*)
let in_channel = open_in file_name

(*
Maintenant, on parse le fichier pour récupérer une liste de chaines de
   caractères. On suppose que les accents sont séparés par des blancs.
*)
let string_inputs =
  let list_as_string = input_line in_channel in

  let rec read_word n max =
    if n=max then ""
    else match list_as_string.[n] with
      | ' ' -> ""
      | c -> let m = read_word (n+1) max in (String.make 1 c)^m
  in
  let rec string_to_list n max =
    if n=max then []
    else match list_as_string.[n] with
      | ' ' -> string_to_list (n+1) max
      | _ -> let m = read_word n max in m :: string_to_list (n+String.length m) max
  in string_to_list 0 (String.length list_as_string)

let _= close_in in_channel

(*
  print_sorted_list :
  (t list -> t list) -> (string -> t) -> (t -> string) -> unit

  Trie la variable globale string_inputs à l'aide de l'algorithme de tri et
  des fonctions de conversion passées en paramètre, et l'imprime sur la
  sortie standard.
*)

let print_sorted_list algotri of_string to_string=
  List.iter
    (fun x->print_string (to_string x^" "))
    (algotri (List.map of_string string_inputs));
  print_newline ()

(*
  On définit ici les modules dont on pourrait avoir besoin.
*)
module EntierBubblesort = Tri.Bubblesort (Ordre.Entier)
module EntierQuicksort = Tri.Quicksort (Ordre.Entier)
module ChaineBubblesort = Tri.Bubblesort (Ordre.Chaine)
module ChaineQuicksort = Tri.Quicksort (Ordre.Chaine)

(*
  On filtre sur les variables type_utilise et algo_utilise pour exécuter la
  fonction précédente avec les bons paramètres.
*)
let _ =
  match type_utilise,algo_utilise with
    |"int","bubble"->
       print_sorted_list
	 (EntierBubblesort.trier)
	 (Ordre.Entier.of_string)
	 (Ordre.Entier.to_string)
    |"string","bubble"->
       print_sorted_list
	 (ChaineBubblesort.trier)
	 (Ordre.Chaine.of_string)
	 (Ordre.Chaine.to_string)
     |"int","qs"->
       print_sorted_list
	 (EntierQuicksort.trier)
	 (Ordre.Entier.of_string)
	 (Ordre.Entier.to_string)
     |"string","qs"->
       print_sorted_list
	 (ChaineQuicksort.trier)
	 (Ordre.Chaine.of_string)
	 (Ordre.Chaine.to_string)
     |_-> print_endline doc; exit 1
	 (* Si un des paramètres n'est pas reconnu on quitte le programme
	    après avoir affiché la documentation. *)
