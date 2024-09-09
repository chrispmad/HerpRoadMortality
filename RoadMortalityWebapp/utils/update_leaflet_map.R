update_leaflet_map = function(map_name,
                              map_shapes,
                              # var_to_color,
                              the_pal,
                              #roads_in_view, culverts_in_view, zoom_reveal_factor,
                              dat){

  m = leafletProxy("mortmap") |>
    leaflet::removeControl("legend") |>
    # leaflet::clearGroup('Parks') |>
    # leaflet::clearGroup('MapShapes') |>
    leaflet::clearGroup('rmdatCircles') |>
    # leaflet::clearMarkers() |>
    leaflet::addPolygons(
      data = map_shapes,
      layerId = ~shape_name,
      label = ~shape_name,
      color = 'grey',
      weight = 2,
      group = 'MapShapes')
#
#   my_pal = reactive({
#     req(!is.null(var_to_color))
#     if(var_to_color == 'species'){
#       the_pal = leaflet::colorFactor(palette = 'Spectral',
#                                      domain = dat$common_name,
#                                      levels = c("Leigh-Anne",
#                                                 "Herpetofauna_Road_Mortality_ExcelSheet_2022",
#                                                 "BC Data Catalogue - SPI",
#                                                 "iNaturalist"))
#     } else {
#       the_pal = leaflet::colorFactor(palette = 'Set1',
#                            domain = dat$data_source)
#     }
#     the_pal
#   })
# if(var_to_color == 'dat_s'){
#   m = m |>
#     leaflet::addLegend(
#       colors = ~unique(col_for_leaf),
#       labels = ~unique(data_source),
#       title = 'LEGEND',
#       data = dat,
#       layerId = 'legend')
# } else {
#   m = m |>
#     leaflet::addLegend(
#       colors = ~unique(col_for_leaf),
#       labels = ~unique(common_names),
#       title = 'LEGEND',
#       data = dat,
#       layerId = 'legend')
# }

  the_leaf_table = leafpop::popupTable(
    dat |>
      sf::st_drop_geometry() |>
      dplyr::mutate(
        `Project/credit` = ifelse(!is.na(inat_credit),
                                  inat_credit,
                                  paste0(proj_name," (ID: ",na.omit(proj_id),")")),
        `Number Mortalities` = case_when(
          !is.na(adult_male) | !is.na(adult_female) | !is.na(juv_male) | !is.na(juv_female) ~ adult_male + adult_female + juv_male + juv_female,
          !is.na(mort_number) ~ as.numeric(mort_number),
          T ~ NA),
        `Number Survivals` = case_when(
          !is.na(alive_number) ~ alive_number,
          T ~ 'Unknown'
        )) |>
      dplyr::select(`Common Name` = common_name,
                    `Scientific Name` = scientific_name,
                    `Time of Event` = date_time,
                    Notes = comments,
                    `Data Source` = data_source,
                    `Project/credit`,
                    `Number Mortalities`,
                    `Number Survivals`)
  )

  m = m |>
    # groupOptions('Parks', zoomLevels = 6:20) |>
    # groupOptions('Roads', zoomLevels = 8:20) |>
    # groupOptions('Culverts', zoomLevels = 8:20) |>
    leaflet::addCircleMarkers(
      color = ~the_pal(color_col),
      radius = 3,
      weight = 10,
      opacity = 0.65,
      options = pathOptions(pane = "marker_layer"),
      label = ~paste0(common_name, " (",scientific_name,", ",date_time,")"),
      popup = lapply(
        the_leaf_table,
        htmltools::HTML
      ),
      # popup = ~lapply(
      #   paste0("<b>Common Name: </b>", common_name,
      #          "<br><b>Scientific Name: </b>",scientific_name,
      #          "<br><b>Time of Event: </b>",date_time,
      #          "<br><b>Notes: </b>",comments,
      #          "<br><b>Data Source: </b>",data_source,
      #          "<br><b>Project / credit: </b>",ifelse(!is.na(inat_credit),
      #                                                 inat_credit,
      #                                                 paste0(proj_name," (ID: ",na.omit(proj_id),")"))
      #   ),
      #   HTML),
      group = 'rmdatCircles',
      data = dat)

    m = m |>
      leaflet::addLegend(
        pal =  the_pal,
        values = ~color_col,
        title = 'LEGEND',
        data = dat,
        layerId = 'legend')

  m
}
