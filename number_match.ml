open Stdlib
open Unix (* DANS LE BUT DE MESURE DE TEMPS (J'AI UTILISÉ CHATGPT) *)

(* JE DÉFINIS LE TYPE QUE CHAQUE CASE PEUT CONTENIR *)
type cellule =
  | Nombre of int           (* UNE CASE PLEINE DOIT CONTENIR UN CHIFFRE ENTRE 1 ET 9 *)
  | VideInitial             (* UNE CASE QUI EST INITIALEMENT VIDE ET REPRÉSENTÉE PAR ('.') *)
  | VideElimine             (* CASE VIDE FORMÉE PAR ÉLIMINATION QUI SERA REMPLIS PAR ('X') *)

type grille = cellule array array (* LA GRILLE EST UN TABLEAU DE TABLEAU DE CASES *)
type position = int * int          (* LA POSITION c'est juste (ligne, colonne). *)
type coup = Elimination of position * position | Ajouter (* SOIT UNE ELIMINATION SOIT UNE RELANCE. *)

(* ON GARDE EN MEMOIRE LE MEILLEUR CHEMIN TROUVÉ À PARTIR DE LA GRILLE INITIALE. *)
type resultat_solution = (coup list * int) option
type cle_grille = string (* LE BUT ICI EST DE CONVERTIR LA GRILLE EN CHAINE DE CARACTERE POUR MÉMORISER SON ÉTAT *)

(* ICI POUR BIEN AFFICHER SUR TERMINAL J'AI UTILISÉ DES COULEURS (ICI JE LES AI PRIS DE CHATGPT) *)
let reinitialiser = "\x1b[0m"
let rouge = "\x1b[31m"
let vert = "\x1b[32m"
let jaune = "\x1b[33m"
let cyan = "\x1b[36m"
let colonnes_defaut = 9

(* --- MÉMORISATION --- *)
let table_memo = Hashtbl.create 50000 (* ON DEFINIT UNE GRANDE TABLE, CE QUI VA NOU PERMETTRE DE STOCKER LES RÉSULTATS DEJA CALCULÉS *)

(* CONVERSION DE LA GRILLE EN CLÉ DE HACHAGE *)
let grille_vers_cle grille =
  let liste_cle =
    Array.to_list grille
    |> List.map Array.to_list
    |> List.flatten
    |> List.map (function
        | Nombre n -> string_of_int n
        | VideInitial -> "."
        | VideElimine -> "X"
      )
  in
  String.concat "" liste_cle

(* ---- OUTILS BASIQUES ---- *)
let copier_grille g = Array.map Array.copy g (* ON FAIT UNE COPIE DE LA GRILLE CE QUI VA NOUS PERMETTRE DE FAIRE DES ESSAIS SANS MODIFIER L'ORGINAL *)
let obtenir_dimensions g = (Array.length g, if Array.length g > 0 then Array.length g.(0) else 0) (* JE RÉCUPÉRE (lignes, colonnes). *)

let obtenir_cellules_aplaties grille =
  (* ICI JE PREND LA LISTE DE TOUTES LES CASES DE LA GRILLE ET JE LES METS DANS UNE SEULE LISTE . *)
  Array.to_list (Array.concat (Array.to_list grille))

let est_cellule_vide = function
  | VideInitial | VideElimine -> true
  | Nombre _ -> false

let est_correspondance n1 n2 = (n1 = n2) || (n1 + n2 = 10) (* VÉRIFICATION SI LES CHIFFRES SONT IDENTIQUES OU LEUR SOMME EST EGALE À 10. *)

let est_coordonnee_valide grille r c =
  let (lignes, colonnes_actuelles) = obtenir_dimensions grille in
  r >= 0 && r < lignes && c >= 0 && c < colonnes_actuelles

let est_vide grille r c =
  est_coordonnee_valide grille r c && est_cellule_vide grille.(r).(c)

(* NETOYAGE DE LIGNE*)
let supprimer_lignes_vides grille colonnes_actuelles =
  (* retirer toute ligne qui ne contient AUCUN chiffre. *)
  let lignes = Array.length grille in
  if lignes = 0 then grille
  else
    let lignes_a_garder =
      Array.to_list grille
      |> List.filter (fun ligne ->
            (* ON GARDE QUE LES CASES QUI ONT AU MOINS DES CASES PLEINE PLEINE *)
            Array.exists (function Nombre _ -> true | _ -> false) ligne
          )
    in

    match lignes_a_garder with
    | [] ->
        Array.make_matrix 0 colonnes_actuelles VideInitial
    | premiere_ligne :: _ ->
        Array.of_list lignes_a_garder (* JE RECONSTRUI LA NOUVELLE GRILLE APRÉS NETTOYAGE. *)

(* FONCTION DE VALIDATION DU CHEMIN *)

(* Ces fonctions vérifient si le chemin entre deux chiffres est dégagé (que des points '.' ou des croix 'X'). *)
let chemin_libre_ligne grille (r, c1) (r', c2) =
  if r <> r' || abs (c1 - c2) < 2 then false
  else
    let debut_c, fin_c = if c1 < c2 then (c1, c2) else (c2, c1) in
    let chemin_est_vide = ref true in
    for c = debut_c + 1 to fin_c - 1 do
      if not (est_vide grille r c) then chemin_est_vide := false
    done;
    !chemin_est_vide

let chemin_libre_colonne grille (r1, c) (r2, c') =
  if c <> c' || abs (r1 - r2) < 2 then false
  else
    let debut_r, fin_r = if r1 < r2 then (r1, r2) else (r2, r1) in
    let chemin_est_vide = ref true in
    for r = debut_r + 1 to fin_r - 1 do
      if not (est_vide grille r c) then chemin_est_vide := false
    done;
    !chemin_est_vide

let chemin_libre_diagonale grille (r1, c1) (r2, c2) =
  let dr = abs (r2 - r1) and dc = abs (c2 - c1) in
  if dr <> dc || dr < 2 then false
  else
    let pas_r = if r2 > r1 then 1 else -1 in
    let pas_c = if c2 > c1 then 1 else -1 in
    let chemin_est_vide = ref true in
    let r = ref (r1 + pas_r) in
    let c = ref (c1 + pas_c) in
    while !r <> r2 do
      if not (est_vide grille !r !c) then chemin_est_vide := false;
      r := !r + pas_r;
      c := !c + pas_c;
    done;
    !chemin_est_vide

let est_chemin_libre_aplat grille (r1, c1) (r2, c2) =
  (* ON VERIFIE LE CHEMIN "PLAT" (ON VA LIRE LA GRILLE DE GAUCHE À DROITE ET DE HAUT EN BAS. *)
  let aplati = obtenir_cellules_aplaties grille in
  let (_, colonnes_actuelles) = obtenir_dimensions grille in
  let idx1 = r1 * colonnes_actuelles + c1 in
  let idx2 = r2 * colonnes_actuelles + c2 in
  let debut_i, fin_i = if idx1 < idx2 then (idx1, idx2) else (idx2, idx1) in

  if abs (idx1 - idx2) < 2 then false
  else
    try
      for i = debut_i + 1 to fin_i - 1 do
        match List.nth aplati i with
        | Nombre _ -> raise Exit (* J'ai trouvé un chiffre sur le chemin : ce n'est pas dégagé. *)
        | _ -> ()
      done;
      true
    with Exit -> false
      | Failure _ -> false

let est_position_valide grille (r1, c1) (r2, c2) =
  (* VERFIFIANT SI DEUX POSITIONS SONT VALIDES POUR UNE ÉLIMINATION *)
  if r1 = r2 && c1 = c2 then false
  else if not (est_coordonnee_valide grille r1 c1 && est_coordonnee_valide grille r2 c2) then false
  else
    let est_voisin = (abs (r1 - r2) <= 1) && (abs (c1 - c2) <= 1) in (* LA VERIFICTION DE SI LES CHIFFRES SONT VOISINS  *)

    let est_chemin_libre =
      (r1 = r2 && chemin_libre_ligne grille (r1, c1) (r2, c2))
      || (c1 = c2 && chemin_libre_colonne grille (r1, c1) (r2, c2))
      || (abs (r1 - r2) = abs (c1 - c2) && chemin_libre_diagonale grille (r1, c1) (r2, c2))
      || est_chemin_libre_aplat grille (r1, c1) (r2, c2)
    in
    est_voisin || est_chemin_libre (* C'est valide s'ils sont voisins OU si le chemin est dégagé. *)

(* ---- LOGIQUE DU JEU ---- *)

let essayer_eliminer grille (r1, c1) (r2, c2) =
  (* Mon travail : tenter d'éliminer une paire. Si ça marche, je remplace les deux cases par 'VideElimine' ('X'). *)
  match grille.(r1).(c1), grille.(r2).(c2) with
  | Nombre n1, Nombre n2 when est_correspondance n1 n2 && est_position_valide grille (r1, c1) (r2, c2) ->
      grille.(r1).(c1) <- VideElimine;
      grille.(r2).(c2) <- VideElimine;
      true
  | _ -> false

(* LOGIQUE DE RELANCE PERSONNALISÉE (X reste vide) *)
let appliquer_relance_personnalisee grille colonnes_actuelles =
  (* LORS DE LA RELANCE ON REMPLIT QUE LES TROUS QUI ONT ETE VIDES DEPUIS LA GRILLE INITIAL *)
  let valeurs_nombres_restants =
    obtenir_cellules_aplaties grille
    |> List.filter_map (function Nombre n -> Some n | _ -> None)
  in

  if valeurs_nombres_restants = [] then grille
  else
    let (lignes, _) = obtenir_dimensions grille in
    let nouvelle_grille = copier_grille grille in

    let nombres_a_ajouter = Array.of_list valeurs_nombres_restants in
    let total_a_ajouter = Array.length nombres_a_ajouter in
    let compteur_ajoutes = ref 0 in

    (* 1. Remplir les trous SEULEMENT de type VideInitial ('.') *)
    for r = 0 to lignes - 1 do
      for c = 0 to colonnes_actuelles - 1 do
        match nouvelle_grille.(r).(c) with
        | VideInitial when !compteur_ajoutes < total_a_ajouter ->
            nouvelle_grille.(r).(c) <- Nombre nombres_a_ajouter.(!compteur_ajoutes);
            incr compteur_ajoutes
        | _ -> () (* J'ignore les 'X' (VideElimine) et les nombres. *)
      done
    done;

    (* 2. S'il y a des nombres en trop, je les ajoute à la fin de la grille, et j'agrandis la grille si besoin. *)
    let restants_a_ajouter = total_a_ajouter - !compteur_ajoutes in
    if restants_a_ajouter > 0 then begin
      let cellules_a_ajouter =
        List.init restants_a_ajouter (fun i ->
            Nombre nombres_a_ajouter.(!compteur_ajoutes + i))
      in

      let cellules_existantes = obtenir_cellules_aplaties nouvelle_grille in
      let liste_nouvelles_cellules = cellules_existantes @ cellules_a_ajouter in

      let total = List.length liste_nouvelles_cellules in
      let nouvelles_lignes = (total + colonnes_actuelles - 1) / colonnes_actuelles in
      let grille_finale = Array.make_matrix nouvelles_lignes colonnes_actuelles VideInitial in

      List.iteri
        (fun i cellule ->
            let r = i / colonnes_actuelles and c = i mod colonnes_actuelles in
            grille_finale.(r).(c) <- cellule)
        liste_nouvelles_cellules;

      grille_finale
    end else
      nouvelle_grille


(* ---- AFFICHAGE ---- *)

let afficher_grille grille ~coordonnees_surlignees =
  (* J'affiche la grille dans le terminal, en utilisant des couleurs pour les chiffres et les 'X'. *)
  let (lignes, colonnes_actuelles) = obtenir_dimensions grille in
  Printf.printf "\n%sGrille Courante (%dx%d)%s\n" cyan lignes colonnes_actuelles reinitialiser;
  for r = 0 to lignes - 1 do
    for c = 0 to colonnes_actuelles - 1 do
      let surligner = List.mem (r, c) coordonnees_surlignees in
      match grille.(r).(c) with
      | VideInitial -> Printf.printf " . "
      | VideElimine -> Printf.printf "%s X %s" rouge reinitialiser (* Affichage clair de la croix *)
      | Nombre n ->
          if surligner then
            Printf.printf "%s%2d%s " jaune n reinitialiser
          else
            Printf.printf "%s%2d%s " vert n reinitialiser
    done;
    print_newline ()
  done;
  print_newline ()

let afficher_coup = function
  (* ON AFFICHE L'ETAPE DE RESOLUTION *)
  | Elimination ((r1, c1), (r2, c2)) ->
      Printf.printf "Elimination: (%d, %d) & (%d, %d)\n" (r1 + 1) (c1 + 1) (r2 + 1) (c2 + 1)
  | Ajouter ->
      Printf.printf "%sRELANCE (Personnalisée : Remplissage des trous '.')%s\n" jaune reinitialiser

(* ---- GÉNÉRATION DE COUPS ET SOLVEUR AVEC MEMOIZATION ---- *)

let toutes_paires_possibles grille =
  (* trouver TOUTES les paires de chiffres qui peuvent être éliminées dans l'état actuel de la grille. *)
  let (lignes, colonnes_actuelles) = obtenir_dimensions grille in
  let total_cellules = lignes * colonnes_actuelles in
  let paires = ref [] in

  let obtenir_coordonnees i = (i / colonnes_actuelles, i mod colonnes_actuelles) in

  for i1 = 0 to total_cellules - 1 do
    let r1, c1 = obtenir_coordonnees i1 in
    match grille.(r1).(c1) with
    | Nombre n1 ->
        for i2 = i1 + 1 to total_cellules - 1 do
          let r2, c2 = obtenir_coordonnees i2 in
          if est_coordonnee_valide grille r2 c2 then (
            match grille.(r2).(c2) with
            | Nombre n2 when est_correspondance n1 n2 && est_position_valide grille (r1, c1) (r2, c2) ->
                paires := ((r1, c1), (r2, c2)) :: !paires
            | _ -> ()
          )
        done
    | _ -> ()
  done;

  (* On trie les paires trouvées. *)
  List.sort
    (fun ((r1, c1), _) ((r3, c3), _) ->
      let idx1 = r1 * colonnes_actuelles + c1 in
      let idx3 = r3 * colonnes_actuelles + c3 in
      compare idx1 idx3
    )
    !paires


(* Solveur Backtracking récursif AVEC MEMOIZATION et Nettoyage de Ligne *)
let rec resoudre_backtracking grille colonnes_actuelles max_ajouts =
  (* La fonction principale : trouver la séquence de coups pour vider la grille. *)
  let cle = grille_vers_cle grille in

  (* 1. Tenter de récupérer le résultat dans la table de mémorisation *)
  (try
      Some (Hashtbl.find table_memo cle) (* Si je connais déjà le résultat pour cette grille, je l'utilise ! *)
    with Not_found -> None)
  |> (function
      | Some resultat -> resultat
      | None ->

        (* Si le nombre de chiffres restants est impair, c'est impossible de gagner. *)
        let nombre_nombres_actifs =
          obtenir_cellules_aplaties grille
          |> List.filter (function Nombre _ -> true | _ -> false)
          |> List.length
        in

        let resultat =
          if nombre_nombres_actifs mod 2 <> 0 then
            None
          else if nombre_nombres_actifs = 0 then
            Some ([], 0) (* J'ai gagné ! Je retourne une solution vide (plus de coups à faire). *)
          else
            let paires = toutes_paires_possibles grille in
            if paires = [] then
              (* Si je n'ai plus de coups d'élimination possibles *)
              if max_ajouts > 0 then begin
                (* ET S'il me reste des relances, j'essaie une relance. *)
                let nouvelle_grille_avec_ajout = appliquer_relance_personnalisee grille colonnes_actuelles in

                if toutes_paires_possibles nouvelle_grille_avec_ajout = [] then
                    None (* Si la relance n'ouvre aucune nouvelle possibilité, je n'insiste pas. *)
                else
                    match resoudre_backtracking nouvelle_grille_avec_ajout colonnes_actuelles (max_ajouts - 1) with
                    | Some (coups, ajouts) -> Some (Ajouter :: coups, ajouts + 1) (* Succès après la relance. *)
                    | None -> None
              end else
                None (* Échec : plus de coups possibles et plus de relances. *)
            else
              (* J'essaie chacune des paires d'élimination possibles. *)
              let rec essayer_chaque_paire = function
                | [] -> None
                | (pos1, pos2) :: reste -> (* Suppression de l'alias inutilisé 'as paire'. *)
                    let grille_copie = copier_grille grille in
                    let (r1, c1) = pos1 in
                    let (r2, c2) = pos2 in

                    if essayer_eliminer grille_copie (r1, c1) (r2, c2) then
                      let grille_apres_elim = supprimer_lignes_vides grille_copie colonnes_actuelles in (* J'applique le NETTOYAGE de ligne après l'élimination. *)

                      match resoudre_backtracking grille_apres_elim colonnes_actuelles max_ajouts with
                      | Some (coups, ajouts) ->
                          Some (Elimination (pos1, pos2) :: coups, ajouts) (* Succès  Je construis la solution. *)
                      | None -> essayer_chaque_paire reste
                    else
                      essayer_chaque_paire reste
              in
              essayer_chaque_paire paires
        in

        (* 3. Stocker le résultat (succès ou échec) dans la table de mémorisation *)
        Hashtbl.add table_memo cle resultat;
        resultat
  )

(* ---- INTERACTION UTILISATEUR ---- *)

let attendre_entree () =
  Printf.printf "\n%sAppuyez sur ENTRÉE pour l'étape suivante...%s" jaune reinitialiser;
  flush Stdlib.stdout; (* J'utilise Stdlib.stdout explicitement pour éviter le conflit de type. *)
  try
    let _ = read_line () in ()
  with End_of_file -> ()

let lire_entier_entree prompt valeur_defaut =
  flush Stdlib.stdout;
  try
    Printf.printf "%s (%d) : " prompt valeur_defaut;
    let ligne = read_line () in
    if ligne = "" then valeur_defaut else int_of_string ligne
  with Failure _ | End_of_file ->
    Printf.printf "%s! Entrée invalide, utilisation de la valeur par défaut (%d).%s\n" rouge valeur_defaut reinitialiser;
    valeur_defaut

let construire_grille_depuis_liste liste colonnes_actuelles =
  (* la fonction prendre la liste des chiffres et construire ma grille initiale (tableau de tableaux). *)
  let total = List.length liste in
  let lignes = (total + colonnes_actuelles - 1) / colonnes_actuelles in
  let g = Array.make_matrix lignes colonnes_actuelles VideInitial in
  List.iteri
    (fun i n ->
      let r = i / colonnes_actuelles and c = i mod colonnes_actuelles in
      g.(r).(c) <- Nombre n)
    liste;
  g

let rec simuler grille colonnes_actuelles = function
  (* Mon travail : parcourir la solution trouvée coup par coup et afficher l'évolution de la grille. *)
  | [] -> grille
  | (Elimination (pos1, pos2)) as c :: reste ->
    let prochaine_grille = copier_grille grille in

    Printf.printf "\n%s--- PRÉVISUALISATION : ---%s" jaune reinitialiser;
    afficher_coup c;
    afficher_grille prochaine_grille ~coordonnees_surlignees:[pos1; pos2];
    attendre_entree ();

    ignore (essayer_eliminer prochaine_grille (fst pos1, snd pos1) (fst pos2, snd pos2));
    let grille_apres_elim = supprimer_lignes_vides prochaine_grille colonnes_actuelles in

    Printf.printf "\n%s--- La nouvelle grille aprés élimination ---%s" cyan reinitialiser;
    afficher_grille grille_apres_elim ~coordonnees_surlignees:[];
    attendre_entree ();

    simuler grille_apres_elim colonnes_actuelles reste

  | Ajouter as c :: reste ->
    let nouvelle_grille_avec_ajout = appliquer_relance_personnalisee grille colonnes_actuelles in

    Printf.printf "\n%s--- Grille avant Relance ---%s" cyan reinitialiser;
    afficher_grille grille ~coordonnees_surlignees:[];
    afficher_coup c;
    attendre_entree ();

    Printf.printf "\n%s--- Grille après Relance ---%s" vert reinitialiser;
    afficher_grille nouvelle_grille_avec_ajout ~coordonnees_surlignees:[];
    attendre_entree ();

    simuler nouvelle_grille_avec_ajout colonnes_actuelles reste

let () =
  Printf.printf "%s Solveur Number Match  %s\n\n" cyan reinitialiser;

  (* 1. On  demande les paramètres à l'utilisateur. *)
  let colonnes = lire_entier_entree "Nombre de colonnes (C)" colonnes_defaut in
  let nombre_elements = lire_entier_entree "Nombre de chiffres initiaux (L)" 42 in
  let max_ajouts = lire_entier_entree "Nombre maximal de relances (Ajouter)" 1 in
  Printf.printf "\n";

  (* 2. On lis les nombres initiaux (ou on'utilise la séquence par défaut). *)
  let entree_defaut = [7; 1; 4; 2; 7; 9; 5; 3; 6; 4; 2; 7; 1; 6; 2; 4; 1; 9; 3; 5; 6; 2; 3; 1; 5; 8; 6; 9; 2; 9; 5; 6; 8; 7; 6; 5; 7; 6; 3; 8; 7; 9] in
  Printf.printf "Entrez %d nombres ou laissez vide pour utiliser la séquence par défaut (%s...) :\n" nombre_elements (String.concat " " (List.map string_of_int (List.filteri (fun i _ -> i < 6) entree_defaut)));
  let nombres =
    try
      let ligne = read_line () in
      if String.trim ligne = "" then entree_defaut
      else
        let parties = String.split_on_char ' ' ligne |> List.filter ((<>) "") in
        List.filter_map (fun s -> try let n = int_of_string s in if n >= 1 && n <= 9 then Some n else None with _ -> None) parties
    with End_of_file -> entree_defaut
  in

  if List.length nombres < nombre_elements then (
    Printf.printf "%sAvertissement: Nombre d'éléments entrés (%d) inférieur au nombre attendu (%d).%s\n" jaune (List.length nombres) nombre_elements reinitialiser
  );

  let grille_initiale = construire_grille_depuis_liste (List.filteri (fun i _ -> i < nombre_elements) nombres) colonnes in

  Printf.printf "%s--- Grille Initiale (%dx%d) ---%s" jaune (Array.length grille_initiale) colonnes reinitialiser;
  afficher_grille grille_initiale ~coordonnees_surlignees:[];

  Printf.printf "%s On lance la recherche des chiffres à eliminer %s\n" cyan reinitialiser;
  let temps_debut = Unix.gettimeofday () in

  (* 3. On lance le solveur et je mesure le temps. *)
  match resoudre_backtracking grille_initiale colonnes max_ajouts with
  | Some (solution, total_ajouts) ->
      let temps_fin = Unix.gettimeofday () in
      let temps_ecoule = temps_fin -. temps_debut in
      Printf.printf "\n%s Et voila on a trouvé la solution en  %d étapes (dont %d relances) en %.3f secondes :%s\n"
        vert (List.length solution) total_ajouts temps_ecoule reinitialiser;

      Printf.printf "\n%s*** SIMULATION PAS À PAS ***%s\n" vert reinitialiser;
      let _ = simuler grille_initiale colonnes solution in

      Printf.printf "\n%s félicitation le jeu et terminé (Mémorisation : %d états)%s\n" vert (Hashtbl.length table_memo) reinitialiser

  | None ->
      let temps_fin = Unix.gettimeofday () in
      let temps_ecoule = temps_fin -. temps_debut in
      Printf.printf "\n%s Aucune solution trouvée avec %d relances maximum en %.3f secondes (Mémorisation : %d états).%s\n" rouge max_ajouts temps_ecoule (Hashtbl.length table_memo) reinitialiser
