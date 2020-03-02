######################################################################
######################### CODE NON FINALISE ##########################
######################################################################

rm(list=ls())

library(sf)
library(tidyverse)
library(maptools)
library(rmapshaper)
library(CARTElette)
library(COGugaison)

# paramètres
annee <- "2019"
# Fichier : AdminExpress 2019
url <- "ftp://Admin_Express_ext:Dahnoh0eigheeFok@ftp3.ign.fr/ADMIN-EXPRESS-COG_2-0__SHP__FRA_WGS84G_2019-09-24.7z.001"


dest <- "ADMIN-EXPRESS-COG_2-0__SHP__FRA_WGS84G_2019-09-24.7z.001"
######################################################################
#################### TELECHARGEMENT DONNEES IGN ######################
######################################################################

#download.file(url, destfile = paste0("source/",dest))
# dézipper
#system('"C:/Program Files (x86)/7-Zip/7z.exe" x -osource source/ADMIN-EXPRESS-COG_2-0__SHP_L93__FRA_2019-05-20.7z')


######################################################################
##################### CHARGEMENT ET DEPLACEMENT DOM ###################
######################################################################

fr_initial <- st_read("../sources_shp/data_IGN/COG2019/source/ADMIN-EXPRESS-COG_2-0__SHP__FRA_2019-09-24/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2019-09-24/ADE-COG_2-0_SHP_WGS84_FR/COMMUNE_CARTO.shp", stringsAsFactors = F)

fr_metro <- fr_initial %>% filter(substr(INSEE_COM,1,2)!="97")

fr_971 <- fr_initial %>% filter(substr(INSEE_COM,1,3)=="971")  %>% CARTElette:::transformation_shp(rot=0, scale=2, shift=c(-2+61.53983,41-16.198))

fr_972 <- fr_initial %>% filter(substr(INSEE_COM,1,3)=="972") %>%   CARTElette:::transformation_shp(rot=0, scale=2, shift=c(0+61.01964,41-14.6548))

fr_973 <- fr_initial %>% filter(substr(INSEE_COM,1,3)=="973") %>%   CARTElette:::transformation_shp(rot=0, scale=0.3, shift=c(2+53.23766,41-3.922017))

fr_974 <- fr_initial %>% filter(substr(INSEE_COM,1,3)=="974") %>%   CARTElette:::transformation_shp(rot=0, scale=2, shift=c(4-55.53254,41+21.1332))
fr_976 <- fr_initial %>% filter(substr(INSEE_COM,1,3)=="976") %>%   CARTElette:::transformation_shp(rot=0, scale=2, shift=c(6-45.14744,41+12.82061))

fr_total <-
  fr_metro %>%
  rbind(fr_971) %>%
  rbind(fr_972) %>%
  rbind(fr_973) %>%
  rbind(fr_974) %>%
  rbind(fr_976) #%>%
  #st_transform(4326)


# Aperçu du résultat
par(mar=c(0,0,0,0))
plot(fr_total %>% filter(substr(INSEE_COM,1,2)%in%c("62","29","64","06","67","2A","97")) %>% select(INSEE_COM))

#Vérification du contenu des communes
setdiff(get(paste0("COG",annee))$CODGEO,fr_total$INSEE_COM)
setdiff(fr_total$INSEE_COM,get(paste0("COG",annee))$CODGEO) #ARRONDISSEMENT PLM SEULEMENT

#Vérification des géométries
problemes <- which(!st_is_valid(fr_total))
if(length(problemes)!=0){
  fr_total <- st_buffer(fr_total,0)
}

#Tout est OK, on peut exporter
st_write(fr_total, paste0("couches_carto/IGN/COG",annee,
"/COM_",annee,"_CARTElette.shp"))

#Export de la carte communale simplifiée
COMMUNES <- fr_total %>% ms_simplify(keep=0.01,keep_shapes=TRUE)

print(object.size(fr_total), units = "auto")
print(object.size(COMMUNES), units = "auto")

plot(COMMUNES %>% filter(substr(INSEE_COM,1,2)%in%c("62","29","64","06","67","2A","97")) %>% select(INSEE_COM))

st_write(COMMUNES, paste0("couches_carto/IGN/COG",annee,
                          "/COM_",annee,"_simplifiee_CARTElette.shp"))


save(COMMUNES,file="CARTElette/data/COMMUNES.RData", compress='xz')

######################################################################
#####################  COUCHES SUPRACOMMUNALES  ######################
######################################################################
library(sf)

COM_CARTElette <- st_read(paste0("couches_carto/IGN/COG",annee,
                                 "/COM_",annee,"_CARTElette.shp"), stringsAsFactors = FALSE) %>%
  filter(substr(INSEE_COM,1,3)!="751" & substr(INSEE_COM,1,4)!="6938" & substr(INSEE_COM,1,3)!="132") %>% #NOUVEAU (arrondissements PLM)
  merge(.,get(paste0("table_supracom_",annee)),by.x="INSEE_COM",by.y="CODGEO",all.x=T)

exporter_couche <- function(couche_communes, maille,annee){

  couche <- CARTElette:::creer_couche(couche_communes, maille,annee)

  st_write(couche, paste0("couches_carto/IGN/COG",annee,
                          "/",maille,"_",annee,"_CARTElette.shp"))
}

liste_mailles <- c("DEP","EPCI","ARR" ,"CV","ZE2010","UU2010","AU2010","BV2012","REG")

for(maille in liste_mailles){
  exporter_couche(COM_CARTElette, maille,annee)
}
