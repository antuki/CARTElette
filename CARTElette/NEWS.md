# CARTElette 1.0.0

## nouveautés

### mai 2019

* ajout du COG2019
* ajout du paramètre enlever_PLM dans la fonction `loadMap()`

### décembre 2019

* `loadMap()` est renommée `charger_carte()`
* nouvelles fonctions : `positionner_DOM_glisser()`, `positionner_DOM_grille()` et `deplacer_DOM()`

# CARTElette 1.0.1

## nouveautés

### mars 2020

* modification du COG2019 avec les nouveaux fichiers de l'IGN avec France metro + DOM en WGS84
* ajout d'une carte COMMUNES intégrée au package R et modification de  `charger_carte()` avec le paramètre  `geometry_simplifiee` pour tenir compte de cette nouveauté
* modification des fonctions pour tenir compte de plusieurs possibilités de projections suite à la nouvelle couche en WGS84
* amélioration de  `transformation_shp()` pour pouvoir faire des rotations et zooms sur un ensemble de géométries et pas uniquement sur un polygone

### mars 2020

* intégration du COG2020 sans mettre à jour le package

## correction de bugs / optimisation du code

# CARTElette 1.0.2

## nouveautés

### juillet 2021

* intégration du COG2021 (nouvelle utilisation de ADMIN-EXPRESS-COG-CARTO plutôt que ADMIN-EXPRESS-COG) et mise à jour du package

## correction de bugs / optimisation du code
