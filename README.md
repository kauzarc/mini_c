### Julien ROLLAND - Magistere info

# Mini-C :

## Travail realisé :

- Version minimale :

  - analyseur lexical
  - analyseur syntaxique
  - vérificateur de types (affiche des message d'erreur basique)

- Extensions :
  - boucle for
  - opérateurs (+, \*, -, <, >, /, %, !, &&, ||, ==)
  - constante "true" et "false"
  - interprète
  - "include" (effectue un "copier/coller" du fichier cible dans le fichier courant. Le chemin est relatif au fichier courant.)
  - afficheur (sous la forme de l'arbre de syntaxe abstraite en ocaml)
  - "bibliotheque standard": `std.mc` (quelque fonction basique : afficher un entier ou un boolean, calculer des puissances.)

## Utilisation :

Le resultat de la compilation doit etre un fichier `main.native`.
Pour lancer le verificateur de type et l'interprète :

```
$ ./main.native path_to_file/file_name.mc
```

### Exemple :

```
$ ./main.native test/dummy.mc
```

Doit afficher :

```
program:
{globals=[];functions=[{name="main";params=[];return=Void;locals=[];code=[]}]}
no type error
evaluation:
success
```

## Compilation :

Pour compiler :

```
$ make
```

Pour netoyer :

```
$ make clean
```

### Dependance :

- menhir
- ocamlbuild
