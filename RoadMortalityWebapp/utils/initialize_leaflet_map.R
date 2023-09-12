# Start up our leaflet map.
initialize_leaflet_map = function(parks){
  renderLeaflet({
    leaflet() %>%
      addTiles(group = 'Streets') %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
      addProviderTiles(providers$CartoDB,  group = "CartoDB") %>%
      envreportutils::add_bc_home_button() %>%
      envreportutils::set_bc_view(zoom = 5) %>%
      addLayersControl(baseGroups = c("CartoDB","Streets","Terrain"),
                       overlayGroups = c("MapShapes","Roads","Parks"),
                       options = layersControlOptions(collapsed = F),
                       position = 'bottomright') |>
      # addPolylines(
      #   color = 'orange',
      #   weight = 5,
      #   label = ~paste0('Mortality event ',related_mortality,', ',road_surface,' ',road_class),
      #   group = 'Roads',
      #   data = roads_in_100m) |>
      addPolygons(label = ~paste0(park_name,", (",park_type,")"),
                  weight = 1,
                  color = 'black',
                  fillColor = 'darkgreen',
                  group = 'Parks',
                  data = parks)
  })
}
