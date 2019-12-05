#' @title Indiquer la position souhaitée des DOM à partir d'une grille géographique
#' @name positionner_DOM_grille
#' @description Indiquer la position souhaitée des DOM à partir d'une grille géographique
#' @details
#' La fonction renvoie une liste de coordonnées pour les futurs centroïdes
#' @references
#' \itemize{
#' \item{\href{http://professionnels.ign.fr/adminexpress}{couches cartographiques Admin-Express (IGN)}}
#' \item{\href{http://professionnels.ign.fr/geofla}{couches cartographiques GEOFLA (IGN)}}}
#' @export
#' @examples
#' \dontrun{
#' positions <- positionner_DOM_grille()
#' positions
#' }
#' @encoding UTF-8
#' @importFrom leaflet leaflet addTiles fitBounds labelOptions highlightOptions leafletOptions
#' @importFrom dplyr filter mutate arrange select
#' @importFrom sf st_transform st_centroid st_coordinates st_drop_geometry st_bbox st_crs
#' @importFrom leafem addFeatures
#' @importFrom stringr str_c
#' @importFrom mapedit selectMap





positionner_DOM_grille <- function(){

  grid <- readRDS("data/grille_france.RDS")


  epsg_affichage <- leafletCRS(crsClass = 'L.Proj.CRS', code = 'EPSG:2154',
                               proj4def = "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs",
                               resolutions = c(65536, 32768, 16384, 8192, 4096, 2048)
  )


  (lf <- leaflet(
    options=
      leafletOptions(maxZoom = 5,
                     crs =  epsg_affichage,
                     worldCopyJump = FALSE
      ))  %>%
      addFeatures(
        data=grid, weight = 1, color = "#000000",
          layerId = ~id,
          label=str_c(grid$id),
          labelOptions= labelOptions(direction = 'auto'),
          highlightOptions = highlightOptions(
            color='#00ff00', bringToFront = TRUE, sendToBack = TRUE)
      ) %>%
  #    addTiles() %>%
      fitBounds(as.numeric(st_bbox(grid)[1]),as.numeric(st_bbox(grid)[2]),as.numeric(st_bbox(grid)[3]),as.numeric(st_bbox(grid)[4]))
  )

  cinq_dom <- selectMap(
    lf,
    styleFalse = list(weight = 1),
    styleTrue = list(weight = 4),
    title = "Selectionnez les 5 DOM."
  )

  suppressWarnings({ #st_transform apres filter ?
    grid_small<- grid %>% filter(id %in% cinq_dom$id)  %>%  st_transform("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs") %>% st_centroid() %>%   mutate(X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2]) %>%
      arrange(match(id, cinq_dom$id)) %>% st_drop_geometry() %>% select(-id)

    grid_small.list <- as.list(as.data.frame(t(grid_small)))
  })




  if(length(grid_small.list)!=5){
    stop("Il faut DU PREMIER COUP sélectionner EXACTEMENT 5 rectangles associés aux 5 DOM.")
  }


  return(grid_small.list)
}


