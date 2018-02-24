######################################################################
######################### CODE NON FINALISE ##########################
######################################################################

rm(list=ls())

# paramètres
annee <- "2015"
# Fichier : GEOFLA 2015
url <- c("https://wxs-telechargement.ign.fr/oikr5jryiph0iwhw36053ptm/telechargement/inspire/GEOFLA_THEME-COMMUNE_2015_2$GEOFLA_2-1_COMMUNE_SHP_LAMB93_FXX_2015-12-01/file/GEOFLA_2-1_COMMUNE_SHP_LAMB93_FXX_2015-12-01.7z",
"https://wxs-telechargement.ign.fr/oikr5jryiph0iwhw36053ptm/telechargement/inspire/GEOFLA_THEME-COMMUNE_2015_2$GEOFLA_2-1_COMMUNE_SHP_UTM20W84GUAD_D971_2015-12-01/file/GEOFLA_2-1_COMMUNE_SHP_UTM20W84GUAD_D971_2015-12-01.7z",
"https://wxs-telechargement.ign.fr/oikr5jryiph0iwhw36053ptm/telechargement/inspire/GEOFLA_THEME-COMMUNE_2015_2$GEOFLA_2-1_COMMUNE_SHP_UTM20W84MART_D972_2015-12-01/file/GEOFLA_2-1_COMMUNE_SHP_UTM20W84MART_D972_2015-12-01.7z",
"https://wxs-telechargement.ign.fr/oikr5jryiph0iwhw36053ptm/telechargement/inspire/GEOFLA_THEME-COMMUNE_2015_2$GEOFLA_2-1_COMMUNE_SHP_UTM22RGFG95_D973_2015-12-01/file/GEOFLA_2-1_COMMUNE_SHP_UTM22RGFG95_D973_2015-12-01.7z",
"https://wxs-telechargement.ign.fr/oikr5jryiph0iwhw36053ptm/telechargement/inspire/GEOFLA_THEME-COMMUNE_2015_2$GEOFLA_2-1_COMMUNE_SHP_RGR92UTM40S_D974_2015-12-01/file/GEOFLA_2-1_COMMUNE_SHP_RGR92UTM40S_D974_2015-12-01.7z",
"https://wxs-telechargement.ign.fr/oikr5jryiph0iwhw36053ptm/telechargement/inspire/GEOFLA_THEME-COMMUNE_2015_2$GEOFLA_2-1_COMMUNE_SHP_RGM04UTM38S_D976_2015-12-01/file/GEOFLA_2-1_COMMUNE_SHP_RGM04UTM38S_D976_2015-12-01.7z"
)

dest <- c("GEOFLA_2-1_COMMUNE_SHP_LAMB93_FXX_2015-12-01.7z",
         "GEOFLA_2-1_COMMUNE_SHP_UTM20W84GUAD_D971_2015-12-01.7z",
         "GEOFLA_2-1_COMMUNE_SHP_UTM20W84MART_D972_2015-12-01.7z",
         "GEOFLA_2-1_COMMUNE_SHP_UTM22RGFG95_D973_2015-12-01.7z",
         "GEOFLA_2-1_COMMUNE_SHP_RGR92UTM40S_D974_2015-12-01.7z",
         "GEOFLA_2-1_COMMUNE_SHP_RGM04UTM38S_D976_2015-12-01.7z"
)

######################################################################
#################### TELECHARGEMENT DONNEES IGN ######################
######################################################################

setwd(paste0(getwd(),"/couches_carto/IGN/COG",annee,"/"))

for(i in 1:length(url)){
  #download.file(url[i],destfile=paste0("source/",dest[i]))
  # protection du site de l'IGN, télécharger à la main et le placer dans source
  # dézipper
  system(paste0('"C:/Program Files (x86)/7-Zip/7z.exe" x -osource source/',dest[i]))
}

######################################################################
##################### CHARGEMENT ET DEPLACEMENT DOM ###################
######################################################################
library(sf)
library(tidyverse)
library(maptools)

transformation_shp <- function(object, rot, scale, shift){
  object %>% elide(rotate = rot) %>%
    elide(scale=scale) %>% 
    elide(shift = shift)     
}


fr_metro <- st_read("./source/GEOFLA_2-1_COMMUNE_SHP_LAMB93_FXX_2015-12-01/GEOFLA/1_DONNEES_LIVRAISON_2015/GEOFLA_2-1_SHP_LAMB93_FR-ED152/COMMUNE/COMMUNE.shp",stringsAsFactors = F) %>%
  st_transform(4326) %>%  as('Spatial') 

fr_971 <- st_read("./source/GEOFLA_2-1_COMMUNE_SHP_UTM20W84GUAD_D971_2015-12-01/GEOFLA/1_DONNEES_LIVRAISON_2015/GEOFLA_2-1_SHP_UTM20W84GUAD_D971-ED152/COMMUNE/COMMUNE.shp",stringsAsFactors = F)%>%
  st_transform(4326) %>% as('Spatial') %>% 
  transformation_shp(rot=0, scale=1, shift=c(-2,41)) 
proj4string(fr_971) <- proj4string(fr_metro)

fr_972 <- st_read("./source/GEOFLA_2-1_COMMUNE_SHP_UTM20W84MART_D972_2015-12-01/GEOFLA/1_DONNEES_LIVRAISON_2015/GEOFLA_2-1_SHP_UTM20W84MART_D972-ED152/COMMUNE/COMMUNE.shp",stringsAsFactors = F)%>%
  st_transform(4326) %>% as('Spatial') %>% 
  transformation_shp(rot=0, scale=1, shift=c(0,41)) 
proj4string(fr_972) <- proj4string(fr_metro)

fr_973 <- st_read("./source/GEOFLA_2-1_COMMUNE_SHP_UTM22RGFG95_D973_2015-12-01/GEOFLA/1_DONNEES_LIVRAISON_2015/GEOFLA_2-1_SHP_UTM22RGFG95_D973-ED152/COMMUNE/COMMUNE.shp",stringsAsFactors = F) %>%
  st_transform(4326) %>% as('Spatial') %>% 
  transformation_shp(rot=0, scale=1, shift=c(2,41)) 
proj4string(fr_973) <- proj4string(fr_metro)

fr_974 <- st_read("./source/GEOFLA_2-1_COMMUNE_SHP_RGR92UTM40S_D974_2015-12-01/GEOFLA/1_DONNEES_LIVRAISON_2015/GEOFLA_2-1_SHP_RGR92UTM40S_D974-ED152/COMMUNE/COMMUNE.shp",stringsAsFactors = F) %>%
  st_transform(4326) %>% as('Spatial') %>% 
  transformation_shp(rot=0, scale=1, shift=c(4,41)) 
proj4string(fr_974) <- proj4string(fr_metro)

fr_976 <- st_read("./source/GEOFLA_2-1_COMMUNE_SHP_RGM04UTM38S_D976_2015-12-01/GEOFLA/1_DONNEES_LIVRAISON_2015/GEOFLA_2-1_SHP_RGM04UTM38S_D976-ED152/COMMUNE/COMMUNE.shp",stringsAsFactors = F) %>%
  st_transform(4326) %>% as('Spatial') %>% 
  transformation_shp(rot=0, scale=1, shift=c(6,41)) 
proj4string(fr_976) <- proj4string(fr_metro)

fr_total <- 
  fr_metro %>%
  rbind(fr_971) %>%
  rbind(fr_972) %>%
  rbind(fr_973) %>%
  rbind(fr_974) %>%
  rbind(fr_976) %>% 
  spTransform(CRS("+init=epsg:2154")) %>% 
  st_as_sf()

# Aperçu du résultat
par(mar=c(0,0,0,0))
plot(fr_total %>% filter(substr(INSEE_COM,1,2)%in%c("62","29","64","06","67","2A","97")) %>% select(INSEE_COM))


#fusionner Paris Lyon Marseille et corriger le code de l'Oudon
fr_total <- left_join(fr_total %>%  
                        mutate(INSEE_COM=ifelse(substr(INSEE_COM,1,2)=="75","75056",INSEE_COM),
                               INSEE_COM=ifelse(substr(INSEE_COM,1,4)=="6938","69123",INSEE_COM),
                               INSEE_COM=ifelse(substr(INSEE_COM,1,3)=="132","13055",INSEE_COM),
                               INSEE_COM=ifelse(INSEE_COM=="14697","14472",INSEE_COM)
                        ) %>% 
                      dplyr::select(INSEE_COM) %>% group_by(INSEE_COM)  %>% summarize(),
                      fr_total %>% st_set_geometry(NULL),
                      by=c("INSEE_COM"="INSEE_COM")) %>%
  select(colnames(fr_total)) %>% mutate(NOM_COM=ifelse(INSEE_COM=="75056","PARIS",NOM_COM),
                                        NOM_COM=ifelse(INSEE_COM=="69123","LYON",NOM_COM),
                                        NOM_COM=ifelse(INSEE_COM=="13055","MARSEILLE",NOM_COM)
  )

fr_total_insee <- fr_total  %>%  mutate(INSEE_COM=ifelse(INSEE_COM=="14472","14697",INSEE_COM)) 

#Vérification du contenu des communes
library(COGugaison)
setdiff(get(paste0("COG",annee))$CODGEO,fr_total$INSEE_COM)
setdiff(fr_total$INSEE_COM,get(paste0("COG",annee))$CODGEO)

setdiff(get(paste0("COG",annee,"_insee"))$CODGEO,fr_total_insee$INSEE_COM)
setdiff(fr_total_insee$INSEE_COM,get(paste0("COG",annee,"_insee"))$CODGEO)

#Tout est OK, on peut exporter 
st_write(fr_total, paste0("COM_",annee,"_CARTElette.shp"))
st_write(fr_total_insee, paste0("COM_",annee,"_insee_CARTElette.shp"))

######################################################################
#####################  COUCHES SUPRACOMMUNALES  ######################
######################################################################

library(sf)
COM_CARTElette <- st_read(paste0("./COM_",annee,"_CARTElette.shp"), stringsAsFactors = FALSE) %>%
  merge(.,get(paste0("table_supracom_",annee)),by.x="INSEE_COM",by.y="CODGEO",all.x=T) 

creer_couche <- function(maille){
  couche <- COM_CARTElette  %>%
    dplyr::select(maille) %>%
    group_by(get(maille))  %>% summarize() %>%
    setNames(nm=c(maille,colnames(.)[-1])) %>% 
    merge(.,get(paste0("libelles_supracom_",annee)) %>% filter(NIVGEO==maille) %>% select(-c(1,4)),by.x=maille,by.y="CODGEO",all.x=T) %>% 
    setNames(nm=c(maille,"nom","geometry"))
  couche <- couche[!couche$nom=="Sans objet",]
  if(maille=="AU2010"){
    couche <- couche[!couche$AU2010%in%c("000","997","998"),]
  }
  st_write(couche, paste0(maille,"_",annee,"_CARTElette.shp"))
}

liste_mailles <- c("DEP","EPCI","ARR" ,"CV","ZE2010","UU2010","AU2010","BV2012","REG")
for(maille in liste_mailles){
  creer_couche(maille)
}

