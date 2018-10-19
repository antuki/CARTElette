# CARTElette <img src="cartelette.png" width=300 align="right" />

Le découpage des territoires français, en particulier les communes, n'est pas un phénomène immuable. Chaque année certaines communes changent de codes, ou bien de nom, fusionnent ou encore se divisent. Certains périmètres supra-communaux changent également, comme celui des cantons qui a été récemment redéfini. C'est à l'Insee que revient le suivi de ces changements afin d'établir chaque année le code officiel géographique (COG).

Ce répertoire vient en complément du package R [antuki/COGugaison](https://github.com/antuki/COGugaison) qui a pour objectif de manipuler des données communales produites à différents millésimes et de les agréger à différents niveaux supra-communaux. 

Il s'agit désormais de créer des couches cartographiques (communales et supra-communales) qui correspondent à la situation du découpage des territoires français (communes et niveaux supra-communaux, France et Outre-mer) au 01 janvier de chaque année (date de référence du code officiel géographique).

Il est également très important de souligner que les couches cartographiques constituées ici sont réalisées à partir des couches publiées par l'IGN et sont modifiées par un programme R (déplacement des DOM afin de constituer des couches à visée statistique).

* COG2017 et 2018 : [ADMIN-EXPRESS](http://professionnels.ign.fr/adminexpress)
* COG antérieurs : [GEOFLA](http://professionnels.ign.fr/geofla)

Un package R est aussi adossé à ce repository. Il permet à ce stade de charger la couche cartographique adaptée à vos données en indiquant l'année du code officiel géographique (COG) ainsi que le niveau géographique (communal ou supra-communal) souhaités


Pour installer le package `CARTElette` et le charger dans R :
 
     devtools::install_github("antuki/CARTElette/CARTElette@RPackage")
     library(CARTElette)

Un exemple en chargeant la couche "sf" des zones d'emplois françaises de 2016 : 

     ZE_sf <- loadMap(COG=2016,nivsupra="ZE2010")
     par(mar=c(0,0,0,0))
     plot(sf::st_geometry(ZE_sf))


