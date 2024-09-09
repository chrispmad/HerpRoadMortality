# Start up our leaflet map.
initialize_leaflet_map = function(){
  renderLeaflet({
    withProgress(
      message = 'Initializing map',
      detail = 'adding parks...',
      value = 0,
      expr = {
        m = leaflet() |>
          addTiles(group = 'Streets') |>
          addProviderTiles(providers$CartoDB,  group = "CartoDB") |>
          envreportutils::add_bc_home_button() |>
          envreportutils::set_bc_view(zoom = 5) |>
          addMapPane("marker_layer", zIndex = 500) |>
          addLayersControl(baseGroups = c("CartoDB","Streets"),
                           overlayGroups = c("MapShapes","Parks","Roads"),
                           options = layersControlOptions(collapsed = F),
                           position = 'bottomright') |>
          addPolygons(
            weight = 1,
            color = 'black',
            fillColor = 'darkgreen',
            label = ~PROTECTED_AREA_NAME,
            group = 'Parks',
            data = parks)

        incProgress(detail = 'adding roads...',
                    amount = 1/3)
        m = m |>
          addPolylines(
            color = 'orange',
            weight = 5,
            label = ~paste0(name_alias, ' (',surface,' ',class,', ID:',dra_id, ')'),
            group = 'Roads',
            data = road_lines) |>
          leaflet::hideGroup('Roads')

        # incProgress(detail = 'adding culverts...',
        #             amount = 1/4)
        # m = m |>
        #   addCircleMarkers(
        #     color = 'darkblue',
        #     label = ~paste0('ID: ',culvert_id, ', ',material,' ',type),
        #     group = 'Culverts',
        #     data = culvert_points)

        incProgress(detail = 'rendering...',
                    amount = 1/3)
        m
      })
  })
}
