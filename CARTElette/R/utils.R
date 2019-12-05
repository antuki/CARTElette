# modification de la fonction editFeatures de mapedit
editFeatures2 <- function (x, ...)
{
  UseMethod("editFeatures2")
}

editFeatures2.sf <- function(
  x,
  map = NULL,
  mergeOrder = c("add", "edit", "delete"),
  record = FALSE,
  viewer = shiny::paneViewer(),
  crs = 4326,
  label = NULL,
  title = "Edit Map",
  editor = c("leaflet.extras", "leafpm"),
  ...
) {

  # store original projection of edited object ----
  orig_proj <- sf::st_crs(x)
  if (is.na(orig_proj)) {
    stop("The CRS of the input object is not set. Aborting. `mapedit` does not currently
         allow editing objects with arbitrary coordinates system. Please set the
         CRS of the input using `sf::st_set_crs()` (for `sf` objects) or `proj4string()
         for `sp` objects", call. = FALSE)
  }

  x$edit_id = as.character(1:nrow(x))

  if (is.null(map)) {
    x = mapview:::checkAdjustProjection(x)
    map = mapview::mapview()@map
    map = leafem::addFeatures(
      map, data=x, layerId=~x$edit_id,
      label=label,
      labelOptions = leaflet::labelOptions(direction="top", offset=c(0,-40)),
      group = "toedit"
    )
    ext = mapview:::createExtent(x)
    map = leaflet::fitBounds(
      map,
       #lng1 = ext[1],
       #lat1 = ext[3],
       #lng2 = ext[2],
       #lat2 = ext[4]
       #ajuster à France entière
        lng1 = -5.141277,
        lat1 = 41,
        lng2 = 9.55,
        lat2 = 51
    )
    map = leafem::addHomeButton(map = map, ext = ext)
  } else {
    if(inherits(map, "mapview")) {
      map = map@map
    }
    map = leafem::addFeatures(
      map, data=x, layerId=~x$edit_id,
      label=label,
      labelOptions = leaflet::labelOptions(direction="top", offset=c(0,-40)),
      group = "toedit"
    )
  }

  # currently we don't have a way to set custom options for leaflet.pm
  # and we will want to customize allowSelfIntersection depending on feature types
  if(inherits(map, "mapview")) map = map@map
  if(editor[1] == "leafpm") {
    # now let's see if any of the features are polygons
    if(any(sf::st_dimension(x) == 2)) {

      # map = leafpm::addPmToolbar(
      #   map,
      #   targetGroup = "toedit",
      #   toolbarOptions = leafpm::pmToolbarOptions(drawCircle = FALSE),
      #   drawOptions = leafpm::pmDrawOptions(allowSelfIntersection = FALSE),
      #   editOptions = leafpm::pmEditOptions(allowSelfIntersection = FALSE),
      #   cutOptions = leafpm::pmCutOptions(allowSelfIntersection = FALSE)
      # )

      map = leafpm::addPmToolbar(
        map,
        targetGroup = "toedit",
        toolbarOptions = leafpm::pmToolbarOptions(drawMarker = FALSE, drawPolygon = FALSE,drawPolyline = FALSE, drawCircle = FALSE, drawRectangle = FALSE,editMode = FALSE, cutPolygon = FALSE, removalMode = FALSE,position = "topright"),
        drawOptions = leafpm::pmDrawOptions(snappable = FALSE, snapDistance = 20, snapMiddle = FALSE,tooltips = FALSE, cursorMarker = FALSE, finishOn = NULL, allowSelfIntersection = TRUE),
        editOptions = leafpm::pmEditOptions(snappable = FALSE, snapDistance = 20,allowSelfIntersection = TRUE, draggable = TRUE, preventMarkerRemoval = FALSE, preventVertexEdit = FALSE),
        cutOptions = leafpm::pmCutOptions(snappable = FALSE, allowSelfIntersection = TRUE,cursorMarker = FALSE)
      )


    }
  }

  crud = editMap2(
    map, targetLayerId = "toedit",
    viewer = viewer, record = record,
    crs = crs, title = title, editor = editor, ...
  )

  merged <- Reduce(
    function(left_sf, op) {
      op <- tolower(op)
      if(op == "add") sf_merge <- crud$finished
      if(op == "edit") sf_merge <- crud$edited
      if(op == "delete") sf_merge <- crud$deleted

      if(is.null(sf_merge)) return(left_sf)

      # will need to rethink this but for now
      #   since we use finished above
      #   only apply edit and delete
      #   where an edit_id is available
      #   meaning only to a feature in the original sf
      if(op %in% c("edit", "delete")) {
        # if layerId column does not exist then all are new features
        #   and should already be handled in finished
        if(!("layerId" %in% colnames(sf_merge))) {
          return(left_sf)
        }
        # ignore any with NA as layerId since these will also be
        #  handled in finished
        sf_merge <- sf_merge[which(!is.na(sf_merge$layerId)),]
      }

      if(nrow(sf_merge) == 0) return(left_sf)

      # eval(call(paste0("merge_", op), left_sf, sf_merge, c("edit_id" = "layerId")), envir = as.environment("package:mapedit")) #modif
      #

      if(op=="edit") {
        mapedit:::merge_edit(left_sf, sf_merge,c("edit_id" = "layerId"))
      } else{
        mapedit:::merge_delete(left_sf, sf_merge,c("edit_id" = "layerId"))
      }




    },
    mergeOrder,
    init = x
  )

  merged <- dplyr::select_(merged, "-edit_id")

  # re-transform to original projection if needed ----
  if (sf::st_crs(merged) != orig_proj) {
    merged <- sf::st_transform(merged, orig_proj)
  }

  # warn if anything is not valid
  if(!all(sf::st_is_valid(merged))) {
    warning("returned features do not appear valid; please inspect closely", call. = FALSE)
  }

  # return merged features
  if(record==TRUE) {
    attr(merged, "recorder") <- attr(crud, "recorder", exact=TRUE)
    attr(merged, "original") <- x
  }

  merged
  }
# modification de la fonction editMod de mapedit
editModAntuki <- function (input, output, session, leafmap, targetLayerId = NULL,
                           sf = TRUE, record = FALSE, crs = 4326, editor = c("leaflet.extras",
                                                                             "leafpm"))
{
  if (is.null(Find(function(cl) {
    cl$method == "addDrawToolbar" || cl$method == "addPmToolbar"
  }, leafmap$x$calls))) {
    if (editor[1] == "leaflet.extras") {

      #utile ?

      # leafmap <- leaflet.extras::addDrawToolbar(leafmap,
      #                                           position="topright", #NEW
      #                                           targetGroup = targetLayerId, polylineOptions = FALSE,
      #                                           polygonOptions = FALSE,
      #                                           circleOptions = FALSE, rectangleOptions = FALSE,
      #                                           markerOptions = FALSE,
      #                                           circleMarkerOptions = FALSE,
      #                                           editOptions = leaflet.extras::editToolbarOptions(edit = TRUE, remove = FALSE, selectedPathOptions = NULL, allowIntersection = FALSE))

    }
    if (editor[1] == "leafpm") {
      # leafmap <- leafpm::addPmToolbar(leafmap, targetGroup = targetLayerId,
      #                                 toolbarOptions = leafpm::pmToolbarOptions(drawCircle = FALSE))

      #### utile ?
      # leafmap <-leafpm::addPmToolbar(
      #   leafmap,
      #   targetGroup = targetLayerId,
      #   toolbarOptions = FALSE,
      #   drawOptions = FALSE,
      #   editOptions = leafpm::pmEditOptions(snappable = FALSE, snapDistance = 20,allowSelfIntersection = TRUE, draggable = TRUE, preventMarkerRemoval = FALSE, preventVertexEdit = FALSE),
      #   cutOptions = FALSE
      # )

    }
  }
  output$map <- leaflet::renderLeaflet({
    leafmap
  })
  featurelist <- shiny::reactiveValues(drawn = list(), edited_all = list(),
                                deleted_all = list(), finished = list())
  recorder <- list()
  EVT_DRAW <- "map_draw_new_feature"
  EVT_EDIT <- "map_draw_edited_features"
  EVT_DELETE <- "map_draw_deleted_features"
  shiny::observeEvent(input[[EVT_DRAW]], {
    featurelist$drawn <- c(featurelist$drawn, list(input[[EVT_DRAW]]))
    if (any(unlist(input[[EVT_DRAW]]$geometry$coordinates) <
            -180) || any(unlist(input[[EVT_DRAW]]$geometry$coordinates) >
                         180))
      mapedit:::insane_longitude_warning()
    featurelist$finished <- c(featurelist$finished, list(input[[EVT_DRAW]]))
  })
  shiny::observeEvent(input[[EVT_EDIT]], {
    edited <- input[[EVT_EDIT]]
    ids <- unlist(lapply(featurelist$finished, function(x) {
      x$properties$`_leaflet_id`
    }))
    lapply(edited$features, function(x) {
      loc <- match(x$properties$`_leaflet_id`, ids)
      if (length(loc) > 0) {
        featurelist$finished[loc] <<- list(x)
      }
    })
    featurelist$edited_all <- c(featurelist$edited_all, list(edited))
  })
  shiny::observeEvent(input[[EVT_DELETE]], {
    deleted <- input[[EVT_DELETE]]
    ids <- unlist(lapply(featurelist$finished, function(x) {
      x$properties$`_leaflet_id`
    }))
    if (editor == "leafpm") {
      deleted <- list(type = "FeatureCollection",
                      features = list(deleted))
    }
    lapply(deleted$features, function(x) {
      loc <- match(x$properties$`_leaflet_id`, ids)
      if (length(loc) > 0) {
        featurelist$finished[loc] <<- NULL
      }
    })
    featurelist$deleted_all <- c(featurelist$deleted_all,
                                 list(deleted))
  })
  if (record == TRUE) {
    lapply(c(EVT_DRAW, EVT_EDIT, EVT_DELETE), function(evt) {
      shiny::observeEvent(input[[evt]], {
        recorder <<- c(recorder, list(list(event = evt,
                                           timestamp = Sys.time(), feature = input[[evt]])))
      })
    })
  }
  returnlist <- shiny::reactive({
    workinglist <- list(drawn = featurelist$drawn, edited = featurelist$edited_all,
                        deleted = featurelist$deleted_all, finished = featurelist$finished)
    if (sf) {
      workinglist <- lapply(workinglist, function(action) {
        if (length(action) == 0) {
          return()
        }
        features <- Reduce(function(left, right) {
          if (right$type == "FeatureCollection") {
            right <- lapply(right$features, identity)
          }
          else {
            right <- list(right)
          }
          c(left, right)
        }, action, init = NULL)
        mapedit:::combine_list_of_sf(lapply(features, mapedit:::st_as_sf.geo_list,
                                            crs = crs))
      })
      recorder <- lapply(recorder, function(evt) {
        feature = mapedit:::st_as_sfc.geo_list(evt$feature, crs = crs)
        list(evt = evt$event, timestamp = evt$timestamp,
             feature = feature)
      })
    }
    if (record == TRUE) {
      attr(workinglist, "recorder") <- recorder
    }
    return(workinglist)
  })
  return(returnlist)
}

# modification de la fonction editMap de mapedit
editMap2 <- function(x, ...) {
  UseMethod("editMap2")
}

editMap2.leaflet <- function(
  x = NULL, targetLayerId = NULL, sf = TRUE,
  ns = "mapedit-edit", record = FALSE, viewer = shiny::paneViewer(),
  crs = 4326,
  title = "Argh",
  editor = c("leaflet.extras", "leafpm"),
  ...
) {
  stopifnot(!is.null(x), inherits(x, "leaflet"))

  stopifnot(
    requireNamespace("leaflet"),
    requireNamespace("leaflet.extras"),
    requireNamespace("shiny"),
    requireNamespace("miniUI")
  )

  ui <- miniUI::miniPage(
    miniUI::miniContentPanel(
      mapedit::editModUI(id = ns, height="97%"),
      height=NULL, width=NULL
    ),
    miniUI::gadgetTitleBar(
      title = title,
      left = miniUI::miniTitleBarCancelButton(inputId="cancel", label="Annuler", primary=FALSE),
      right = miniUI::miniTitleBarButton("done", "Terminer", primary = TRUE)
    ),
    shiny::tags$script(shiny::HTML(
      "
      // close browser window on session end
      $(document).on('shiny:disconnected', function() {
      // check to make sure that button was pressed
      //  to avoid websocket disconnect caused by some other reason than close
      if(
      Shiny.shinyapp.$inputValues['cancel:shiny.action'] ||
      Shiny.shinyapp.$inputValues['done:shiny.action']
      ) {
      window.close()
      }
      })
      "
    ))
    )

  server <- function(input, output, session) {
    crud <- shiny::callModule(
      editModAntuki,
      ns,
      x,
      targetLayerId = targetLayerId,
      sf = sf,
      record = record,
      crs = crs,
      editor = editor
    )

    shiny::observe({crud()})

    shiny::observeEvent(input$done, {
      shiny::stopApp(
        crud()
      )
    })

    # if browser viewer and user closes tab/window
    #  then Shiny does not stop so we will stopApp
    #  when a session ends.  This works fine unless a user might
    #  have two sessions open.  Closing one will also close the
    #  other.
    session$onSessionEnded(function() {
      # should this be a cancel where we send NULL
      #  or a done where we send crud()
      shiny::stopApp(shiny::isolate(crud()))
    })

    shiny::observeEvent(input$cancel, { shiny::stopApp (NULL) })
  }

  shiny::runGadget(
    ui,
    server,
    viewer =  viewer,
    stopOnCancel = FALSE
  )
}

# modification de la fonction selectMap de mapedit
selectMap.leaflet <- function(
  x = NULL,
  styleFalse = list(fillOpacity = 0.2, weight = 1, opacity = 0.4),
  styleTrue = list(fillOpacity = 0.7, weight = 3, opacity = 0.7),
  ns = "mapedit-select",
  viewer = shiny::paneViewer(),
  title = "Select features",
  ...
) {
  stopifnot(!is.null(x), inherits(x, "leaflet"))

  stopifnot(
    requireNamespace("leaflet"),
    requireNamespace("leaflet.extras"),
    requireNamespace("shiny"),
    requireNamespace("miniUI")
  )

  ui <- miniUI::miniPage(
    miniUI::miniContentPanel(
      mapedit::selectModUI(id = ns, height = "100%"),
      height=NULL, width=NULL
    ),
    gadgetTitleBar(title = title),
    shiny::tags$script(shiny::HTML(
      "
      // close browser window on session end
      $(document).on('shiny:disconnected', function() {
      // check to make sure that button was pressed
      //  to avoid websocket disconnect caused by some other reason than close
      if(
      Shiny.shinyapp.$inputValues['cancel:shiny.action'] ||
      Shiny.shinyapp.$inputValues['done:shiny.action']
      ) {
      window.close()
      }
      })
      "
    ))
    )

  server <- function(input, output, session) {
    selections <- shiny::callModule(
      mapedit:::selectMod,
      ns,
      x,
      styleFalse = styleFalse,
      styleTrue = styleTrue
    )

    shiny::observe({selections()})

    shiny::observeEvent(input$done, {
      shiny::stopApp(
        selections()
      )
    })

    shiny::observeEvent(input$cancel, { shiny::stopApp (NULL) })
    session$onSessionEnded(function() {
      shiny::stopApp(shiny::isolate(selections()))
    })
  }

  shiny::runGadget(
    ui,
    server,
    viewer = viewer,
    stopOnCancel = FALSE
  )
  }

editMap2.mapview <- function(
  x = NULL, targetLayerId = NULL, sf = TRUE,
  ns = "mapedit-edit", record = FALSE, viewer = shiny::paneViewer(),
  crs = 4326,
  title = "Edit Map",
  editor = c("leaflet.extras", "leafpm"),
  ...
) {
  stopifnot(!is.null(x), inherits(x, "mapview"), inherits(x@map, "leaflet"))

  editMap2.leaflet(
    x@map, targetLayerId = targetLayerId, sf = sf,
    ns = ns, viewer = viewer, record = TRUE, crs = crs,
    title = title,
    editor = editor
  )
}
# modification de la fonction gadgetTitleBar de miniUI
gadgetTitleBar <- function (title)
{
  left = miniUI::miniTitleBarCancelButton(inputId="cancel", label="Annuler", primary=FALSE)
  right = miniUI::miniTitleBarButton("done","Terminer", primary = TRUE)
  htmltools::attachDependencies(

    htmltools::withTags({


      shiny::div(style="margin-left:10px ; margin-right:10px; background:#FFFFFF; vertical-align: middle;",
          shiny::HTML(
            paste0(
              shiny::div(style="float: left; width: 15%;",
                  shiny::tagAppendAttributes(left, class = "pull-left")
              ),
              shiny::div(style="float: left; width: 70%; text-align:center;",shiny::HTML("<center>Veuillez sélectionner les 5 emplacements souhaités pour les 5 DOM <b>dans cet ordre</b> : Guadeloupe, Martinique, Guyane, Reunion et Mayotte.</center>
")),
              shiny::div(style="float: right; width: 15%;",
                  shiny::tagAppendAttributes(right, class = "pull-right")
              )
            )
          )
      )




    })
    , miniUI:::gadgetDependencies()

  )

}

## création de la grille et des données utiles pour le package
construire_grille <- function(){

  REG_sf <- charger_carte(COG=2018,nivsupra="REG")

  grid <- REG_sf %>%
    #    st_transform(2154) %>% #Déjà en Lambert 93
    filter(!REG%in%c("01","02","03","04","06")) %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_buffer(dist = 200000) %>%
    st_make_grid(n = c(30, 30)) %>%
    st_sf() %>%
    mutate(id = 1:nrow(.)) %>%
    st_difference(REG_sf %>%  filter(!REG%in%c("01","02","03","04","06")) %>%  summarize) %>%
    st_transform(4326)

  saveRDS(grid, file = "grille_france.RDS")

 }

#construire_grille()

transformation_shp <- function(objet, rot, scale, shift){

  objet_g = sf::st_geometry(objet)
  # plot(objet_g, border = 'grey')
  suppressWarnings({ cntrd = sf::st_centroid(objet_g) })
  rotation = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  objet_g2 = (objet_g - cntrd) * rotation(rot) * scale + cntrd + shift
  objet_g2 = sf::st_sf(objet %>% st_drop_geometry(), geometry = objet_g2, crs = sf::st_crs(objet))
  # plot(objet_g2, add = TRUE)
  # plot(cntrd + shift, col = 'red', add = TRUE, cex = .5)
  return(objet_g2)

  #Example
  # toto <- transformation_shp(objet=st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE), rot=pi, scale=1, shift=c(1,2))
  # plot(toto)
}
