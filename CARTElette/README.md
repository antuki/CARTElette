# CARTElette <img src="man/figures/logo.png" width=200 align="right" />

*Afin de mieux connaître les utilisateurs des packages COGugaison et CARTElette et de mieux répondre à vos besoins, merci de répondre à cette rapide enquête en ligne* : https://antuki.github.io/2019/11/08/opinion_package/

Le découpage des territoires français, en particulier les communes, n'est pas un phénomène immuable. Chaque année certaines communes changent de codes, ou bien de nom, fusionnent ou encore se divisent. Certains périmètres supra-communaux changent également, comme celui des cantons qui a été récemment redéfini. C'est à l'Insee que revient le suivi de ces changements afin d'établir chaque année le code officiel géographique (COG).

Ce répertoire vient en complément du package R [antuki/COGugaison](https://github.com/antuki/COGugaison) qui a pour objectif de manipuler des données communales produites à différents millésimes et de les agréger à différents niveaux supra-communaux. 

Il s'agit désormais de créer des couches cartographiques (communales et supra-communales) qui correspondent à la situation du découpage des territoires français (communes et niveaux supra-communaux, France et Outre-mer) au 01 janvier de chaque année (date de référence du code officiel géographique).

Il est également très important de souligner que les couches cartographiques constituées ici sont réalisées à partir des couches publiées par l'IGN et sont modifiées par un programme R (déplacement des DOM afin de constituer des couches à visée statistique).

* à partir du COG2021 : [ADMIN-EXPRESS-COG-CARTO](https://geoservices.ign.fr/adminexpress#telechargementCog) (IGN) également disponible sur [data.gouv](https://www.data.gouv.fr/fr/datasets/admin-express/)
* COG2017 à 2020 : [ADMIN-EXPRESS-COG](https://geoservices.ign.fr/adminexpress#telechargementCog) (IGN) également disponible sur [data.gouv](https://www.data.gouv.fr/fr/datasets/admin-express/)
* COG antérieurs : GEOFLA (IGN)

Un package R est aussi adossé à ce repository. Il permet de *charger la couche cartographique* adaptée à vos données en indiquant l'*année* du code officiel géographique (COG) ainsi que le *niveau géographique* (communal ou supra-communal) souhaités ainsi que de *déplacer les départements d'outre-mer*.


Pour installer le package `CARTElette` et le charger dans R :
 
```r 
remotes::install_github("antuki/CARTElette/CARTElette@RPackage")
library(CARTElette)
```

Un exemple en chargeant la couche "sf" des zones d'emplois françaises de 2016 : 

```r
ZE_sf <- charger_carte(COG=2016,nivsupra="ZE2010")
par(mar=c(0,0,0,0))
plot(sf::st_geometry(ZE_sf))
```

### Licence

CARTElette est un répertoire et un package R open-source qui intègre des données (couches cartographiques) provenant d'[Admin-Express COG de l'IGN](https://geoservices.ign.fr/adminexpress#telechargementCog). Les données CARTElette sont disponibles sous la licence libre [Open Data Commons Open Database License (ODbL)](http://opendatacommons.org/licenses/odbl/1.0/). Tous les droits sur les contenus individuels ont pour licence la [Database Contents Licence](http://opendatacommons.org/licenses/dbcl/1.0/).

Vous êtes donc libre de copier, distribuer, transmettre et adapter les données de ce répertoire et package R, à condition que vous créditiez CARTElette de cette manière « © ADMIN EXPRESS COG (IGN) et projet CARTElette (http://github.com/antuki/CARTElette) ». Si vous modifiez ou utilisez ces données dans d’autres oeuvres dérivées, vous ne pouvez distribuer celles-ci que sous la même licence. 

Le code du package est quant à lui sous licence open-source [GPL-3](CARTElette/LICENSE). 

