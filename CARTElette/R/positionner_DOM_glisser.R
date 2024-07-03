#' @title Indiquer la position souhaitée des DOM en les glissant à partir d'une interface graphique
#' @name positionner_DOM_glisser
#' @description Indiquer la position souhaitée des DOM en les glissant à partir d'une interface graphique
#' @param objet est un fond de carte de la France (métropolitaine et 5 DOM) de type sf. Les niveaux géographiques possibles sont ceux du package CARTElette c'est-à-dire :
#' - "DEP" : départements
#' - "REG" : régions
#' - "EPCI" : EPCI au 01/01/20XX
#' - "ARR" : arrondissements au 01/01/20XX
#' - "CV" : cantons-villes au 01/01/20XX (avant 2024)
#' - "CANOV" : cantons-ou-villes au 01/01/20XX (après 2024)
#' - "ZE2010" : zones d'emploi 2010 (avant 2020)
#' - "ZE2020" : zones d'emploi 2020 (après 2020)
#' - "UU2010" : unités urbaines 2010 (avant 2020)
#' - "UU2020" : unités urbaines 2020 (après 2020)
#' - "AU2010" : aires urbaines 2010 (avant 2020)
#' - "AAV2020" : aires d'attraction des villes 2020 (après 2020)
#' - "BV2012" : bassins de vie 2012 (avant 2023)
#' - "BV2022" : bassins de vie 2012 (après 2023)
#' @param nivsupra est une chaîne de caractères qui indique le nom du niveau supra-communal concerné. Cette chaîne de caractère doit également correspondre à la colonne de l'objet sf contenant les codes géographiques. Il peut s'agir de :
#' - "DEP" : départements
#' - "REG" : régions
#' - "EPCI" : EPCI au 01/01/20XX
#' - "ARR" : arrondissements au 01/01/20XX
#' - "CV" : cantons-villes au 01/01/20XX (avant 2024)
#' - "CANOV" : cantons-ou-villes au 01/01/20XX (après 2024)
#' - "ZE2010" : zones d'emploi 2010 (avant 2020)
#' - "ZE2020" : zones d'emploi 2020 (après 2020)
#' - "UU2010" : unités urbaines 2010 (avant 2020)
#' - "UU2020" : unités urbaines 2020 (après 2020)
#' - "AU2010" : aires urbaines 2010 (avant 2020)
#' - "AAV2020" : aires d'attraction des villes 2020 (après 2020)
#' - "BV2012" : bassins de vie 2012 (avant 2023)
#' - "BV2022" : bassins de vie 2012 (après 2023)
#' @details
#' La fonction renvoie une liste de coordonnées pour les futurs centroïdes
#' @references
#' \itemize{
#' \item{\href{https://geoservices.ign.fr/adminexpress#telechargementCog}{couches cartographiques ADMIN-EXPRESS-COG (IGN)}}}
#' @export
#' @examples
#' \dontrun{
#' ze <- charger_carte(COG = 2021, nivsupra = "ZE2020")
#' positions <- positionner_DOM_glisser(objet=ze)
#' positions
#' }
#' @encoding UTF-8
#' @importFrom dplyr summarize mutate pull
#' @importFrom sf st_centroid st_coordinates st_drop_geometry
#' @importFrom rmapshaper ms_simplify

positionner_DOM_glisser <- function(objet, nivsupra=colnames(objet)[1]){

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
  } else if(nivsupra=="CANOV"){
    fr_971 <- objet %>% filter(substr(CANOV,1,3)=="971")
    fr_972 <- objet %>% filter(substr(CANOV,1,3)=="972")
    fr_973 <- objet %>% filter(substr(CANOV,1,3)=="973")
    fr_974 <- objet %>% filter(substr(CANOV,1,3)=="974")
    fr_976 <- objet %>% filter(substr(CANOV,1,3)=="976")
  } else if(nivsupra=="BV2012"){
    fr_971 <- objet %>% filter(substr(BV2012,1,3)=="971")
    fr_972 <- objet %>% filter(substr(BV2012,1,3)=="972")
    fr_973 <- objet %>% filter(substr(BV2012,1,3)=="973")
    fr_974 <- objet %>% filter(substr(BV2012,1,3)=="974")
    fr_976 <- NULL
  } else if(nivsupra=="BV2022"){
    fr_971 <- objet %>% filter(substr(BV2022,1,3)=="971")
    fr_972 <- objet %>% filter(substr(BV2022,1,3)=="972")
    fr_973 <- objet %>% filter(substr(BV2022,1,3)=="973")
    fr_974 <- objet %>% filter(substr(BV2022,1,3)=="974")
    fr_976 <- NULL
  } else if(nivsupra=="ARR"){
    fr_971 <- objet %>% filter(substr(ARR,1,3)=="971")
    fr_972 <- objet %>% filter(substr(ARR,1,3)=="972")
    fr_973 <- objet %>% filter(substr(ARR,1,3)=="973")
    fr_974 <- objet %>% filter(substr(ARR,1,3)=="974")
    fr_976 <- NULL
  } else if(nivsupra=="EPCI"){
    fr_971 <- objet %>% filter(EPCI %in%  COGugaison::table_supracom_2024 %>% filter(REG=="01") %>% select(EPCI) %>% pull())
    fr_972 <- objet %>% filter(EPCI %in%  COGugaison::table_supracom_2024 %>% filter(REG=="02") %>% select(EPCI) %>% pull())
    fr_973 <- objet %>% filter(EPCI %in%  COGugaison::table_supracom_2024 %>% filter(REG=="03") %>% select(EPCI) %>% pull())
    fr_974 <- objet %>% filter(EPCI %in%  COGugaison::table_supracom_2024 %>% filter(REG=="04") %>% select(EPCI) %>% pull())
    fr_976 <- objet %>% filter(EPCI %in%  COGugaison::table_supracom_2024 %>% filter(REG=="06") %>% select(EPCI) %>% pull())
  }

  fr_971 <- fr_971 %>% summarize
  fr_972 <- fr_972 %>% summarize
  fr_973 <- fr_973 %>% summarize
  fr_974 <- fr_974 %>% summarize
  if(nrow(fr_976)!=0){
    fr_976 <- fr_976 %>% summarize
  }

  fr_DOM <- fr_971 %>%
    rbind(fr_972) %>%
    rbind(fr_973) %>%
    rbind(fr_974) %>%
    rbind(fr_976) %>%
    ms_simplify(keep=0.1,keep_shapes=TRUE)
    #st_simplify(preserveTopology = TRUE, dTolerance = 1000)

  suppressWarnings({
    fr_DOM <- fr_DOM %>%
      editFeatures2(editor="leafpm",title="Deplacez les 5 DOM et appuyez sur Terminer")

    coord <- fr_DOM %>% st_centroid() %>% mutate(X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2]) %>% select(X,Y) %>%  st_drop_geometry()

    })


  coord.list <- as.list(as.data.frame(t(coord)))

  return(coord.list)
}
