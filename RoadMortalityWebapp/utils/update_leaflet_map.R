update_leaflet_map = function(map_name, map_shapes, dat){
  leafletProxy("mortmap") %>%
    removeControl("legend") %>%
    leaflet::clearGroup('test') %>%
    leaflet::clearGroup('MapShapes') %>%
    leaflet::clearGroup('rmdatCircles') %>%
    leaflet::clearMarkers() %>%
    addPolygons(
      data = map_shapes,
      layerId = ~shape_name,
      label = ~shape_name,
      color = 'grey',
      weight = 2,
      group = 'MapShapes') %>%
    leaflet::addCircleMarkers(
      color = 'purple',
      radius = 3,
      weight = 10,
      opacity = 0.8,
      label = ~paste0(common_name, " (",scientific_name,", ",date_time,")"),
      popup = ~lapply(
        paste0("<b>Common Name: </b>", common_name,
               "<br><b>Scientific Name: </b>",scientific_name,
               "<br><b>Time of Event: </b>",date_time,
               "<br><b>Notes: </b>",comments,
               "<br><b>Data Source: </b>",data_source,
               "<br><b>Project / credit: </b>",ifelse(!is.na(inat_credit),
                                                      inat_credit,
                                                      paste0(proj_name," (ID: ",na.omit(proj_id),")"))
        ),
        HTML),
      group = 'rmdatCircles',
      data = dat) #%>%
  # addLegend(pal = mypal,
  #           values = ~common_name,
  #           title = 'LEGEND',
  #           data = rmdat_f(),
  #           layerId = 'legend')
}
