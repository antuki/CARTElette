#' @title Modifier une couche cartographique de type sf en indiquant la position souhaitée des DOM
#' @name deplacer_DOM
#' @description Modifier une couche cartographique de type sf en indiquant la position souhaitée des DOM
#' @param objet est un fond de carte de la France (métropolitaine et 5 DOM) de type sf. Les niveaux géographiques possibles sont ceux du package CARTElette c'est-à-dire :
#' - "DEP" : départements
#' - "REG" : régions
#' - "EPCI" : EPCI au 01/01/20XX
#' - "ARR" : arrondissements au 01/01/20XX
#' - "CV" : cantons-villes au 01/01/20XX
#' - "ZE2010" : zones d'emploi 2010 (avant 2020)
#' - "ZE2020" : zones d'emploi 2020 (après 2020)
#' - "UU2010" : unités urbaines 2010 (avant 2020)
#' - "UU2020" : unités urbaines 2020 (après 2020)
#' - "AU2010" : aires urbaines 2010 (avant 2020)
#' - "AAV2020" : aires d'attraction des villes 2020 (après 2020)
#' - "BV2012" : bassins de vie 2012
#' @param nivsupra est une chaîne de caractères qui indique le nom du niveau supra-communal concerné. Cette chaîne de caractère doit également correspondre à la colonne de l'objet sf contenant les codes géographiques. Il peut s'agir de :
#' - "DEP" : départements
#' - "REG" : régions
#' - "EPCI" : EPCI au 01/01/20XX
#' - "ARR" : arrondissements au 01/01/20XX
#' - "CV" : cantons-villes au 01/01/20XX
#' - "ZE2010" : zones d'emploi 2010 (avant 2020)
#' - "ZE2020" : zones d'emploi 2020 (après 2020)
#' - "UU2010" : unités urbaines 2010 (avant 2020)
#' - "UU2020" : unités urbaines 2020 (après 2020)
#' - "AU2010" : aires urbaines 2010 (avant 2020)
#' - "AAV2020" : aires d'attraction des villes 2020 (après 2020)
#' - "BV2012" : bassins de vie 2012
#' @param positions_type est un type de position proposé parmi les positions par défaut :
#' - "topleft" : DOM positionnés en haut à gauche
#' @param positions est une liste de positions pour les DOM (à indiquer dans l'ordre :  Guadeloupe, Martinique, Guyane, Réunion et Mayotte) . Il peut s'agir par exemple de résultats des fonctions positionner_DOM_grille ou encore positionner_DOM_glisser. Par défaut vaut NULL (cf. positions_type)
#' @param rotations vecteur de 5 angles au format trigonométrique (pi/2 pour 90°...) qui correspondent aux rotations souhaitées pour les DOM (à indiquer dans l'ordre :  Guadeloupe, Martinique, Guyane, Réunion et Mayotte). Par défaut vaut rep(0,5) (pas de rotation).
#' @param zooms vecteur de 5 zooms qui correspondent aux grossissements souhaités pour les DOM (à indiquer dans l'ordre :  Guadeloupe, Martinique, Guyane, Réunion et Mayotte). Par défaut vaut rep(1,5) (pas de zoom).
#' @param projection projection de la couche cartographique à modifier. vaut NULL si la projection souhaitée est WGS84 ("+proj=longlat +datum=WGS84 +no_defs"). Sinon peut être devinée en faisant sf::st_crs(couche_sf)$proj4string
#' @details
#' La fonction renvoie un objet de type sf
#' @references
#' \itemize{
#' \item{\href{https://geoservices.ign.fr/adminexpress#telechargementCog}{couches cartographiques ADMIN-EXPRESS-COG (IGN)}}}
#' @export
#' @examples
#' \dontrun{
#' # Exemple 1 : choisir precisement la position des DOM
#' ze <- charger_carte(COG = 2021, nivsupra = "ZE2020")
#' #positions <- positionner_DOM_glisser(objet=ze)
#' #positions <- positionner_DOM_grille(projection = st_crs(ze)$proj4string)
#' positions <- list(c(-5.074931, 46.920490), c(-6.768008, 49.571175), c(-2.65836, 45.08238),
#'                  c( -6.195586, 45.084970), c(4.948049, 41.193759))
#' ze_final <- deplacer_DOM(objet = ze, positions = positions)
#' par(mar=c(0, 0, 0, 0))
#' plot(sf::st_geometry(ze_final))
#'
#' # Exemple 2 : choisir des positions de DOM par defaut
#' ze_final_2 <- deplacer_DOM(objet = ze, positions_type = "topleft")
#' par(mar = c(0, 0, 0, 0))
#' plot(sf::st_geometry(ze_final_2))
#'
#' # Exemple 3 : changer les zooms des DOM et les faire pivoter
#' ze_final_3 <- deplacer_DOM(objet = ze, positions_type = "topleft",
#' rotations = c(90,180,90,-90,0), zooms = c(1,1,2,0.5,1))
#' par(mar = c(0,0,0,0))
#' plot(sf::st_geometry(ze_final_3))
#' }
#' @encoding UTF-8
#' @importFrom dplyr summarize mutate
#' @importFrom sf st_simplify st_centroid st_coordinates st_drop_geometry st_geometry

deplacer_DOM <- function(objet, nivsupra=colnames(objet)[1], positions_type=c("topleft"), positions=NULL,rotations=rep(0,5),zooms=rep(1,5),projection = st_crs(objet)$proj4string){

  if(is.null(positions)){

    ### Lambert 93
    if(projection=="+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs"){
      if(positions_type=="topleft"){
        vect <- c(7042660.43,6896577.03,6750493.63,6604410.23,6458326.83)
        positions <- lapply(vect,function(x){return(c(-8607.131,x))})
      }
      if(positions_type=="topright"){
      }
    }

    # WGS 84
    if(projection=="+proj=longlat +datum=WGS84 +no_defs"){
      if(positions_type=="topleft"){
        vect <- c(50.5,49,47.5,46,44.5)
        positions <- lapply(vect,function(x){return(c(-6.7,x))})
      }
      if(positions_type=="topright"){
      }
    }



  }




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
  } else if(nivsupra=="ZE2020"){
    fr_971 <- objet %>% filter(substr(ZE2020,1,2)=="01")
    fr_972 <- objet %>% filter(substr(ZE2020,1,2)=="02")
    fr_973 <- objet %>% filter(substr(ZE2020,1,2)=="03")
    fr_974 <- objet %>% filter(substr(ZE2020,1,2)=="04")
    fr_976 <- objet %>% filter(substr(ZE2020,1,2)=="06")
  } else if(nivsupra=="AU2010"){
    fr_971 <- objet %>% filter(substr(AU2010,1,2)=="9A")
    fr_972 <- objet %>% filter(substr(AU2010,1,2)=="9B")
    fr_973 <- objet %>% filter(substr(AU2010,1,2)=="9C")
    fr_974 <- objet %>% filter(substr(AU2010,1,2)=="9D")
    fr_976 <- objet %>% filter(substr(AU2010,1,2)=="9F")
  } else if(nivsupra=="AAV2020"){
    fr_971 <- objet %>% filter(substr(AAV2020,1,2)=="9A")
    fr_972 <- objet %>% filter(substr(AAV2020,1,2)=="9B")
    fr_973 <- objet %>% filter(substr(AAV2020,1,2)=="9C")
    fr_974 <- objet %>% filter(substr(AAV2020,1,2)=="9D")
    fr_976 <- objet %>% filter(substr(AAV2020,1,2)=="9F")
  } else if(nivsupra=="UU2010"){
    fr_971 <- objet %>% filter(substr(UU2010,1,2)=="9A")
    fr_972 <- objet %>% filter(substr(UU2010,1,2)=="9B")
    fr_973 <- objet %>% filter(substr(UU2010,1,2)=="9C")
    fr_974 <- objet %>% filter(substr(UU2010,1,2)=="9D")
    fr_976 <- objet %>% filter(substr(UU2010,1,2)=="9F")
  } else if(nivsupra=="UU2020"){
    fr_971 <- objet %>% filter(substr(UU2020,1,2)=="9A")
    fr_972 <- objet %>% filter(substr(UU2020,1,2)=="9B")
    fr_973 <- objet %>% filter(substr(UU2020,1,2)=="9C")
    fr_974 <- objet %>% filter(substr(UU2020,1,2)=="9D")
    fr_976 <- objet %>% filter(substr(UU2020,1,2)=="9F")
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
    fr_976 <- NULL
  } else if(nivsupra=="ARR"){
    fr_971 <- objet %>% filter(substr(ARR,1,3)=="971")
    fr_972 <- objet %>% filter(substr(ARR,1,3)=="972")
    fr_973 <- objet %>% filter(substr(ARR,1,3)=="973")
    fr_974 <- objet %>% filter(substr(ARR,1,3)=="974")
    fr_976 <- NULL
  } else if(nivsupra=="EPCI"){
    fr_971 <- objet %>% filter(EPCI %in%  COGugaison::table_supracom_2021 %>% filter(REG=="01") %>% select(EPCI) %>% pull())
    fr_972 <- objet %>% filter(EPCI %in%  COGugaison::table_supracom_2021 %>% filter(REG=="02") %>% select(EPCI) %>% pull())
    fr_973 <- objet %>% filter(EPCI %in%  COGugaison::table_supracom_2021 %>% filter(REG=="03") %>% select(EPCI) %>% pull())
    fr_974 <- objet %>% filter(EPCI %in%  COGugaison::table_supracom_2021 %>% filter(REG=="04") %>% select(EPCI) %>% pull())
    fr_976 <- objet %>% filter(EPCI %in%  COGugaison::table_supracom_2021 %>% filter(REG=="06") %>% select(EPCI) %>% pull())
  }

  dom <- c(fr_971 %>% select(nivsupra) %>% st_drop_geometry() %>% pull,
           fr_972 %>% select(nivsupra) %>% st_drop_geometry() %>% pull,
           fr_973 %>% select(nivsupra) %>% st_drop_geometry() %>% pull,
           fr_974 %>% select(nivsupra) %>% st_drop_geometry() %>% pull,
           fr_976 %>% select(nivsupra) %>% st_drop_geometry() %>% pull
  )
  fr_metro <- objet %>% filter(!get(nivsupra)%in%dom)

  #coord_971 <- reg_sf_2%>% filter(REG=="01") %>% st_centroid() %>%  st_coordinates()
  #coord_971 <- c(308999,6049735)
  #coord_972 <- c(485508.6, 6051548)
  #coord_973 <- c(647740, 6044184)
  #coord_974 <- c(826625.8, 6035281)
  #coord_976 <- c(980377.1, 6049355)

  suppressWarnings({
  coord_971 <- fr_971 %>% summarize %>% st_centroid() %>%  st_coordinates()
  coord_972 <- fr_972 %>% summarize%>% st_centroid() %>%  st_coordinates()
  coord_973 <- fr_973 %>% summarize%>% st_centroid() %>%  st_coordinates()
  coord_974 <- fr_974 %>% summarize%>% st_centroid() %>%  st_coordinates()
  coord_976 <- fr_976 %>% summarize %>% st_centroid() %>%  st_coordinates()
  })

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
