#' @title Modifier une couche cartographique de type sf en indiquant la position souhaitée des DOM
#' @name deplacer_DOM
#' @description Modifier une couche cartographique de type sf en indiquant la position souhaitée des DOM
#' @param objet est un fond de carte de la France (métropolitaine et 5 DOM) de type sf. Les niveaux géographiques possibles sont ceux du package CARTElette c'est-à-dire :
#' - "DEP" : départements
#' - "REG" : régions
#' - "EPCI" : EPCI au 01/01/20XX
#' - "ARR" : arrondissements au 01/01/20XX
#' - "CV" : cantons-villes au 01/01/20XX
#' - "ZE2010" : zones d'emploi 2010
#' - "UU2010" : unités urbaines 2010
#' - "AU2010" : aires urbaines 2010
#' - "BV2012" : bassins de vie 2012
#' @param nivsupra est une chaîne de caractères qui indique le nom du niveau supra-communal concerné. Cette chaîne de caractère doit également correspondre à la colonne de l'objet sf contenant les codes géographiques. Il peut s'agir de :
#' - "DEP" : départements
#' - "REG" : régions
#' - "EPCI" : EPCI au 01/01/20XX
#' - "ARR" : arrondissements au 01/01/20XX
#' - "CV" : cantons-villes au 01/01/20XX
#' - "ZE2010" : zones d'emploi 2010
#' - "UU2010" : unités urbaines 2010
#' - "AU2010" : aires urbaines 2010
#' - "BV2012" : bassins de vie 2012
#' @param positions_type est un type de position proposé parmi les positions par défaut :
#' - "topleft" : DOM positionnés en haut à gauche
#' @param positions est une liste de positions pour les DOM (à indiquer dans l'ordre :  Guadeloupe, Martinique, Guyane, Réunion et Mayotte) . Il peut s'agir par exemple de résultats des fonctions positionner_DOM_grille ou encore positionner_DOM_glisser. Par défaut vaut NULL (cf. positions_type)
#' @param rotations vecteur de 5 angles au format trigonométrique (pi/2 pour 90°...) qui correspondent aux rotations souhaitées pour les DOM (à indiquer dans l'ordre :  Guadeloupe, Martinique, Guyane, Réunion et Mayotte). Par défaut vaut rep(0,5) (pas de rotation).
#' @param zooms vecteur de 5 zooms qui correspondent aux grossissements souhaités pour les DOM (à indiquer dans l'ordre :  Guadeloupe, Martinique, Guyane, Réunion et Mayotte). Par défaut vaut rep(1,5) (pas de zoom).
#' @details
#' La fonction renvoie un objet de type sf
#' @references
#' \itemize{
#' \item{\href{http://professionnels.ign.fr/adminexpress}{couches cartographiques Admin-Express (IGN)}}
#' \item{\href{http://professionnels.ign.fr/geofla}{couches cartographiques GEOFLA (IGN)}}}
#' @export
#' @examples
#' \dontrun{
#' # Exemple 1 : choisir précisément la position des DOM
#' ze <- charger_carte(COG=2019,nivsupra="ZE2010")
#' #positions <- positionner_DOM_glisser(objet=ze)
#' #positions <- positionner_DOM_grille()
#' positions <- list(c(87279.52,6701636.96),c(204282.6,6497512.2),c(95610.91,6295414.72),c( 1015730,6093224),c(823583.9,6154353.7))
#' ze_final <- deplacer_DOM(objet = ze, positions = positions)
#' par(mar=c(0,0,0,0))
#' plot(sf::st_geometry(ze_final))
#'
#' # Exemple 2 : choisir des positions de DOM par défaut
#' ze_final_2 <- deplacer_DOM(objet = ze, positions_type="topleft")
#' par(mar=c(0,0,0,0))
#' plot(sf::st_geometry(ze_final_2))
#'
#' # Exemple 3 : changer les zooms des DOM et les faire pivoter
#' reg <- charger_carte(COG=2019,nivsupra="REG")
#' reg_final <- deplacer_DOM(objet = reg, positions_type = "topleft",rotations=c(pi/2,pi,pi/2,pi,0),zooms=c(2,1,1,1,1))
#' par(mar=c(0,0,0,0))
#' plot(sf::st_geometry(reg_final))
#' }
#' @encoding UTF-8
#' @importFrom dplyr summarize mutate
#' @importFrom sf st_simplify st_centroid st_coordinates st_drop_geometry

deplacer_DOM <- function(objet, nivsupra=colnames(objet)[1], positions_type=c("topleft"), positions=NULL,rotations=rep(0,5),zooms=rep(1,5)){

  if(is.null(positions)){
    if(positions_type=="topleft"){
      vect <- c(7042660.43,6896577.03,6750493.63,6604410.23,6458326.83)
      #-75062.99
      positions <- lapply(vect,function(x){return(c(-8607.131,x))})

    }
  }


  # coord_971 <- REG_sf %>% filter(REG=="01") %>% st_centroid() %>%  st_coordinates()
  coord_971 <- c(308999,6049735)
  coord_972 <- c(485508.6, 6051548)
  coord_973 <- c(647740, 6044184)
  coord_974 <- c(826625.8, 6035281)
  coord_976 <- c(980377.1, 6049355)

  if(nivsupra=="REG"){
    fr_971 <- objet %>% filter(REG=="01")
    fr_972 <- objet %>% filter(REG=="02")
    fr_973 <- objet %>% filter(REG=="03")
    fr_974 <- objet %>% filter(REG=="04")
    fr_976 <- objet %>% filter(REG=="06")
  } else if(nivsupra=="ZE2010"){
    fr_971 <- objet %>% filter(substr(ZE2010,1,2)=="01")
    fr_972 <- objet %>% filter(substr(ZE2010,1,2)=="02")
    fr_973 <- objet %>% filter(substr(ZE2010,1,2)=="03")
    fr_974 <- objet %>% filter(substr(ZE2010,1,2)=="04")
    fr_976 <- objet %>% filter(substr(ZE2010,1,2)=="06")
  } else if(nivsupra=="AU2010"){
    fr_971 <- objet %>% filter(substr(AU2010,1,2)=="9A")
    fr_972 <- objet %>% filter(substr(AU2010,1,2)=="9B")
    fr_973 <- objet %>% filter(substr(AU2010,1,2)=="9C")
    fr_974 <- objet %>% filter(substr(AU2010,1,2)=="9D")
    fr_976 <- objet %>% filter(substr(AU2010,1,2)=="9F")
  } else if(nivsupra=="UU2010"){
    fr_971 <- objet %>% filter(substr(UU2010,1,2)=="9A")
    fr_972 <- objet %>% filter(substr(UU2010,1,2)=="9B")
    fr_973 <- objet %>% filter(substr(UU2010,1,2)=="9C")
    fr_974 <- objet %>% filter(substr(UU2010,1,2)=="9D")
    fr_976 <- objet %>% filter(substr(UU2010,1,2)=="9F")
  } else if(nivsupra=="DEP"){
    fr_971 <- objet %>% filter(DEP=="971")
    fr_972 <- objet %>% filter(DEP=="972")
    fr_973 <- objet %>% filter(DEP=="973")
    fr_974 <- objet %>% filter(DEP=="974")
    fr_976 <- objet %>% filter(DEP=="976")
  } else if(nivsupra=="COM"){
    fr_971 <- objet %>% filter(substr(COM,1,3)=="971")
    fr_972 <- objet %>% filter(substr(COM,1,3)=="972")
    fr_973 <- objet %>% filter(substr(COM,1,3)=="973")
    fr_974 <- objet %>% filter(substr(COM,1,3)=="974")
    fr_976 <- objet %>% filter(substr(COM,1,3)=="976")
  } else if(nivsupra=="CV"){
    fr_971 <- objet %>% filter(substr(CV,1,3)=="971")
    fr_972 <- objet %>% filter(substr(CV,1,3)=="972")
    fr_973 <- objet %>% filter(substr(CV,1,3)=="973")
    fr_974 <- objet %>% filter(substr(CV,1,3)=="974")
    fr_976 <- objet %>% filter(substr(CV,1,3)=="976")
  } else if(nivsupra=="BV2012"){
    fr_971 <- objet %>% filter(substr(BV2012,1,3)=="971")
    fr_972 <- objet %>% filter(substr(BV2012,1,3)=="972")
    fr_973 <- objet %>% filter(substr(BV2012,1,3)=="973")
    fr_974 <- objet %>% filter(substr(BV2012,1,3)=="974")
    fr_976 <- objet %>% filter(substr(BV2012,1,3)=="976")
  } else if(nivsupra=="ARR"){
    fr_971 <- objet %>% filter(substr(ARR,1,3)=="971")
    fr_972 <- objet %>% filter(substr(ARR,1,3)=="972")
    fr_973 <- objet %>% filter(substr(ARR,1,3)=="973")
    fr_974 <- objet %>% filter(substr(ARR,1,3)=="974")
    #fr_976 <- objet %>% filter(substr(ARR,1,3)=="976") #pas pour Mayotte
  } else if(nivsupra=="EPCI"){
    fr_971 <- objet %>% filter(EPCI %in%  COGugaison::table_supracom_2019 %>% filter(REG=="01") %>% select(EPCI) %>% pull())
    fr_972 <- objet %>% filter(EPCI %in%  COGugaison::table_supracom_2019 %>% filter(REG=="02") %>% select(EPCI) %>% pull())
    fr_973 <- objet %>% filter(EPCI %in%  COGugaison::table_supracom_2019 %>% filter(REG=="03") %>% select(EPCI) %>% pull())
    fr_974 <- objet %>% filter(EPCI %in%  COGugaison::table_supracom_2019 %>% filter(REG=="04") %>% select(EPCI) %>% pull())
    fr_976 <- objet %>% filter(EPCI %in%  COGugaison::table_supracom_2019 %>% filter(REG=="06") %>% select(EPCI) %>% pull())

  }

  dom <- c(fr_971 %>% select(nivsupra) %>% st_drop_geometry() %>% pull,
           fr_972 %>% select(nivsupra) %>% st_drop_geometry() %>% pull,
           fr_973 %>% select(nivsupra) %>% st_drop_geometry() %>% pull,
           fr_974 %>% select(nivsupra) %>% st_drop_geometry() %>% pull,
           fr_976 %>% select(nivsupra) %>% st_drop_geometry() %>% pull
  )
  fr_metro <- objet %>% filter(!get(nivsupra)%in%dom)


  fr_971 <- fr_971 %>%  transformation_shp(rot=rotations[1],scale=zooms[1],shift=positions[[1]] - coord_971)
  fr_972 <- fr_972 %>%  transformation_shp(rot=rotations[2],scale=zooms[2],shift=positions[[2]] - coord_972)
  fr_973 <- fr_973 %>%  transformation_shp(rot=rotations[3],scale=zooms[3],shift=positions[[3]] - coord_973)
  fr_974 <- fr_974 %>%  transformation_shp(rot=rotations[4],scale=zooms[4],shift=positions[[4]] - coord_974)
  fr_976 <- fr_976 %>%  transformation_shp(rot=rotations[5],scale=zooms[5],shift=positions[[5]] - coord_976)


  fr_total <-
    fr_metro  %>%
    rbind(fr_971) %>%
    rbind(fr_972) %>%
    rbind(fr_973) %>%
    rbind(fr_974) %>%
    rbind(fr_976) %>%
    arrange(match(get(nivsupra), objet %>% st_drop_geometry() %>% select(1) %>% pull()))

  return(fr_total)

}

# REG : 2 seuls chiffres 01...
# ZE2010 : 2 premiers chiffres 01...
# AU2010 : 2 premiers chiffres 9A,9B,9C,9D,9F
# UU2010 : 2 premiers chiffres 9A,9B,9C,9D,9F
# DEP : 3 seuls chiffres : 971...
# COM : 3 premiers chiffres : 971
# CV : 3 premiers chiffres 971...
# BV2012 : 3 premiers chiffres 971
# ARR : 3 premiers chiffres 971... SAUF MAYOTTE
# EPCI : pas de critère facile, passer par table_supracom
