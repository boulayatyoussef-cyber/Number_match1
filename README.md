# 🧮 Solveur OCaml de Grille Numérique

Ce projet implémente un solveur pour un jeu de grille numérique en **OCaml**, conçu pour être exécuté dans un terminal.  
Le programme utilise une approche de **backtracking avec mémorisation** pour trouver toutes les solutions possibles et optimiser la recherche.

---

## 📋 Utilisation du Programme

Le solveur s’exécute dans un terminal et demande certains paramètres à l’utilisateur.

### Compilation

```bash
ocamlfind ocamlc -package unix -linkpkg -o solveur numberr.ml
Exécution
bash
Copier le code
./solveur
Interaction Utilisateur
Nombre de colonnes (C) : largeur de la grille initiale (par défaut 9)

Nombre de chiffres initiaux (L) : nombre total d’éléments à placer (par défaut 42)

Nombre maximal de relances (Ajouter) : nombre maximum de fois où l’opération Ajouter est autorisée (par défaut 1)

L’utilisateur peut entrer une séquence de chiffres initiale ou utiliser la séquence par défaut. Le programme affichera ensuite la solution pas à pas.

🛠️ Structures de Données et Justification
Représentation de la Cellule (cellule)
ocaml
Copier le code
type cellule = Nombre of int | VideInitial | VideElimine
Trois états possibles : chiffre actif (Nombre), vide initial (VideInitial), vide créé par élimination (VideElimine)

Simplifie la fonction de relance qui ne considère que les cases VideInitial

Représentation de la Grille (grille)
ocaml
Copier le code
type grille = cellule array array
Tableau de tableaux pour un accès direct et rapide aux coordonnées (r,c)

Crucial pour vérifier la validité des coups

Mémorisation (table_memo et cle_grille)
ocaml
Copier le code
type cle_grille = string
let table_memo = Hashtbl.create N
Chaque état de la grille est converti en chaîne unique (cle_grille)

Table de hachage pour stocker le résultat des états déjà explorés

Optimise la complexité du solveur

⚙️ Fonctions Clés
Fonctions d’utilitaires et validation
copier_grille : copie profonde de la grille

est_position_valide : vérifie si deux positions (r1,c1) et (r2,c2) peuvent être appariées

supprimer_lignes_vides : retire les lignes sans Nombre pour compacter la grille

Génération de Coups
toutes_paires_possibles : génère toutes les paires de coups valides

appliquer_relance_personnalisee : ajoute des chiffres uniquement dans les cases VideInitial et agrandit la grille si nécessaire

Solveur Principal
resoudre_backtracking : explore toutes les combinaisons, applique les coups d’élimination et gère les relances avec mémorisation

🏁 Preuve de Terminaison
Copier le code
M(G) = (Nombre de Nombre actifs) + (Nombre de relances restantes)
Élimination : retire 2 cases Nombre, M(G) diminue strictement

Relance : augmente éventuellement le nombre de Nombre, mais les relances restantes diminuent

La mesure M(G) décroît ou est limitée, garantissant la terminaison.
La mémorisation empêche de revisiter des états déjà explorés, éliminant tout risque de boucle infinie.

📈 Analyse de Complexité
Soient N le nombre de cellules et L le nombre initial de chiffres.

Fonctions intermédiaires
grille_vers_cle : O(N)

est_position_valide : O(N)

toutes_paires_possibles : O(N^3)

Complexité du Solveur
Sans mémorisation : O(B^P * N^3), exponentielle

Avec mémorisation : O(V * N^3), optimisée

V : nombre maximum de grilles uniques

B : facteur de branchement O(N^2)

P : profondeur maximale = nombre d’éliminations + relances

