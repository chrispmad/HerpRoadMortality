library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(bs4Dash)
library(bslib)
library(bcdata)
library(plotly)

# Define UI for application that draws a histogram
ui <- bs4Dash::bs4DashPage(
  dark = T,
  header = bs4Dash::dashboardHeader(
    title = div(
      h4(
        HTML('Herpetofauna Road Mortality Spatial Tool')
        ),
      style = 'text-align:center;'
      )
  ),
  sidebar = dashboardSidebar(
    collapsed = F,
    width = '30%',
    h3("Filters"),
    uiOutput('group_sel_UI'),
    checkboxInput(inputId = 'want_species_sel_UI',
                  label = "Enable Species Filter",
                  value = F),
    uiOutput('spec_sel_UI'),
    selectizeInput(
      inputId = 'choose_spatial_containers',
      label = 'Select Spatial Divisions',
      choices = c('Natural Resource Regions',
                  'Natural Resource Districts',
                  'Ecoprovinces',
                  'Ecoregions',
                  'Ecosections'),
      selected = 'Natural Resource Regions'
    ),
    h5("Click Map to Select Shape",style = 'text-align:center;'),
    div(textOutput('shape_selected'), style = 'text-align:center;'),
    div(
      shiny::actionButton(inputId = 'reset_sel_button',
                        label = 'Reset Shape Selection'),
      style = 'margin-left:75px;'
    ),
    h3("Summaries"),
    fluidRow(
      column(width = 6,
             valueBoxOutput('total_records',
                            width = 12)
             ),
      column(width = 6,
             valueBoxOutput('number_dist_species',
                            width = 12)
             )
    ),
    plotOutput('data_time_hist', height = '250px')
  ),
  body = bs4Dash::dashboardBody(
    leafletOutput('mortmap',height = '600px')
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Load data.
  rmdat = read_sf('www/herp_mortality_data.gpkg') %>%
    st_transform(crs = 4326)
  kml_rmdat = read_sf('www/KML_roadmort.gpkg') %>%
    st_transform(crs = 4326)
  nr_regions = read_sf('www/nr_regions.gpkg') %>%
    st_transform(crs = 4326)
  nr_regions = read_sf('www/nr_regions.gpkg') %>%
    st_transform(crs = 4326)
  nr_districts = read_sf('www/nr_districts.gpkg') %>%
    st_transform(crs = 4326)
  ecoprovs = read_sf('www/ecoprovinces.gpkg') %>%
    st_transform(crs = 4326)
  ecosects = read_sf('www/ecosections.gpkg') %>%
    st_transform(crs = 4326)
  ecoregions = read_sf('www/ecoregions.gpkg') %>%
    st_transform(crs = 4326)
  parks = read_sf('www/parks_simplified.gpkg') %>%
    st_transform(crs = 4326) %>%
    rename(park_name = PROTECTED_AREA_NAME,
           park_type = PROTECTED_AREA_TYPE)
  roads_in_100m = read_sf('www/roads_within_100m.gpkg') %>%
    st_transform(crs = 4326)

  # Add Date version of character Date column.
  rmdat = rmdat %>%
    mutate(Date = as.Date(OBSERVATIO))

  # Render UI elements that depend on columns from the datasets.
  # i.
  species_vector = rmdat %>%
    dplyr::select(SPECIES_EN) %>%
    arrange(SPECIES_EN) %>%
    distinct() %>%
    pull(SPECIES_EN)

  # ii.
  output$group_sel_UI = renderUI({
    checkboxGroupInput(
      inputId = 'group_selector',
      label = 'Classes to Include',
      inline = T,
      choices = c("Amphibia",
                  "Reptilia"),
      selected = c("Amphibia","Reptilia")
    )
  })

  # iii.
  output$spec_sel_UI = renderUI({
    if(input$want_species_sel_UI == F) return(NULL)
    selectizeInput(
      inputId = 'species_selector',
      label = 'Species Selector',
      choices = species_vector,
      multiple = F,
      selected = species_vector
    )
  })

  # Render summary widgets
  # i.
  output$total_records = renderValueBox({
    valueBox(value = nrow(rmdat_f()),
             subtitle = 'Total Records',
             width = 12,
             col = 'success',
             gradient = F)
  })

  # ii.
  output$number_dist_species = renderValueBox({
    valueBox(value = length(unique(rmdat_f()$SPECIES_EN)),
             subtitle = 'Distinct Species',
             width = 12,
             col = 'warning')
    })

  # iii. Histogram of # records by year
  output$data_time_hist = renderPlot({
    dat_histogram = rmdat_f() %>%
      ggplot() +
      geom_histogram(aes(OBSERVAT_1),
                     fill = 'white') +
      labs(y = 'Number of Records',
           x = 'Year',
           title = 'Data Distribution By Year') +
      scale_y_continuous(breaks = scales::breaks_pretty()) +
      theme_dark() +
      theme(
        title = element_text(colour = 'white', size = 15),
        axis.text = element_text(colour = 'white',size = 13),
        plot.background = element_rect(fill = 'black'),
        panel.background = element_rect(fill = 'black'))

    dat_histogram
    # ggplotly(dat_histogram)
  })

  # Select which shapes (e.g. regions/districts) to map

  map_shapes = reactive({
    if(input$choose_spatial_containers == 'Natural Resource Regions'){
      return(nr_regions)
    }
    if(input$choose_spatial_containers == 'Natural Resource Districts'){
      return(nr_districts)
    }
    if(input$choose_spatial_containers == 'Ecoprovinces'){
      return(ecoprovs)
    }
    if(input$choose_spatial_containers == 'Ecoregions'){
      return(ecoregions)
    }
    if(input$choose_spatial_containers == 'Ecosections'){
      return(ecosects)
    }
  })

  # Add functionality to select data points by regions/districts. Either dropdown or click area on map.

  # Set up a reactive value that stores a district's name upon user's click
  click_shape <- reactiveVal('Province')

  # Watch for a click on the leaflet map. Once clicked...

  # 1. Update Leaflet map.
  observeEvent(input$mortmap_shape_click, {
    # Capture the info of the clicked polygon. We use this for filtering.
    click_shape(input$mortmap_shape_click$id)
    # Toggle the sidebar to be closed. Note: this skips the actual input$leaflet_sidebar variable.
    # addClass(selector = "body", class = "sidebar-collapse")
    # session$sendCustomMessage("leaflet_sidebar", 'FALSE')
    # manual_sidebar_tracker('closed')
  })

  observeEvent(input$reset_sel_button, {
    click_shape('Province')
  })

  # Reactive version of dataset.

  rmdat_f = reactive({
    dat_intermediate = rmdat %>%
      filter(CLASS_NAME %in% input$group_selector)

    # Spatial match with whatever Province shapes user has chosen.
    dat_intermediate = dat_intermediate %>%
      st_join(map_shapes(), st_intersects)

    if(input$want_species_sel_UI){
      dat_intermediate = dat_intermediate %>%
        filter(SPECIES_EN %in% input$species_selector)
    }
    if(click_shape() != "Province"){
      dat_intermediate = dat_intermediate %>%
        filter(shape_name %in% click_shape())
    }
    return(dat_intermediate)
  })

  # Filtered version of roads.
  roads_in_100m_f = reactive({
    roads_in_100m %>%
      st_join(map_shapes(), st_intersects) %>%
      filter(shape_name %in% click_shape())
  })

  # Filtered version of parks
  parks_f = reactive({
    parks %>%
      st_join(map_shapes(), st_intersects) %>%
      filter(shape_name %in% click_shape())
  })

  # Filtering shape, for highlight in leaflet.
  selected_shape = reactive({
    if(click_shape() == 'Province'){return(
      # No selection? Just make a fake polygon placeholder
      # so leaflet can still render
      st_as_sf(data.frame(lat = c(49,49.1,49),
                           lon = c(-130,-130.1,-130.2)),
               coords = c("lon","lat"), crs = 4326) %>%
        summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON") %>%
        mutate(highlightcolour = 'transparent',
               shape_name = "")
      )
    }
    if(click_shape() != 'Province'){
      return(map_shapes() %>%
      filter(shape_name %in% click_shape()) %>%
      mutate(highlightcolour = 'yellow')
      )
    }
  })

  # Using selected shape to query roads from road atlas... experimental. Might take too long.
  roads_in_shape = reactive({
    data = tryCatch(
      expr = bcdc_query_geodata('digital-road-atlas-dra-master-partially-attributed-roads') %>%
        filter(NUMBER_OF_LANES >= 4) %>%
        filter(ROAD_SURFACE %in% c("paved","rough")) %>%
        # filter(ROAD_CLASS %in% c("local","highway","arterial","yield","collector","freeway")) %>%
        filter(INTERSECTS(selected_shape() %>% st_transform(crs = 3005))) %>%
        collect() %>%
        st_transform(crs = 4326) %>%
        dplyr::select(DIGITAL_ROAD_ATLAS_LINE_ID, FEATURE_TYPE, SEGMENT_LENGTH_2D, ROAD_SURFACE, ROAD_CLASS, DATA_CAPTURE_DATE) %>%
        setNames(snakecase::to_snake_case(names(.))) %>%
        mutate(my_col = 'darkred'),
      error = function(e) {
        data.frame(lat = c(49,49.1), lon = c(-131,-131.1)) %>%
          st_as_sf(coords = c('lon','lat'), crs = 4326) %>%
          summarise(geometry = st_combine(geometry)) %>%
          st_cast("LINESTRING") %>%
          mutate(my_col = 'transparent')
      }
    )
    return(data)
      })

  # Make leaflet
  output$mortmap = renderLeaflet({
    leaflet() %>%
      addTiles(group = 'Streets') %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
      addProviderTiles(providers$CartoDB,  group = "CartoDB") %>%
      envreportutils::add_bc_home_button() %>%
      envreportutils::set_bc_view(zoom = 5) %>%
      addLayersControl(baseGroups = c("CartoDB","Streets","Terrain","Satellite"),
                       overlayGroups = c("MapShapes","Roads","Parks"),
                       options = layersControlOptions(collapsed = F),
                       position = 'bottomright')
      # leaflet::hideGroup(group = 'Parks')
  })

  mypal = colorFactor(palette = 'Set2',
                domain = rmdat$CLASS_NAME)

  output$shape_selected = renderText({
    paste0("Selected: ",click_shape())
  })

  observe({
    leafletProxy("mortmap") %>%
      removeControl("legend") %>%
      # leaflet::removeShape(layerId = 'shapes') %>%
      # leaflet::removeMarker(layerId = 'markers_test') %>%
      leaflet::clearShapes() %>%
      leaflet::clearMarkers() %>%
      addPolygons(
        data = selected_shape(),
        label = ~shape_name,
        layerId = ~highlightcolour,
        color = ~highlightcolour,
        weight = 3,
        group = 'test') %>%
      addPolygons(
        data = map_shapes(),
        layerId = ~shape_name,
        label = ~shape_name,
        col = 'grey',
        weight = 2,
        group = 'MapShapes') %>%
      addPolygons(label = ~paste0(park_name,", (",park_type,")"),
                  # layerId = 'parks'
                  weight = 1,
                  color = 'black',
                  fillColor = 'darkgreen',
                  group = 'Parks',
                  data = parks_f()
      ) %>%
      addPolylines(color = ~my_col,
                   weight = 3,
                   data = roads_in_shape()) %>%
      leaflet::addCircleMarkers(
                                color = ~mypal(CLASS_NAME),
                                radius = 3,
                                weight = 10,
                                opacity = 0.8,
                                label = ~paste0(SPECIES_EN, " (",SCIENTIFIC,",",Date,"): ",SIGN_OR__1,", ",OBSERVAT_5),
                                # layerId = 'markers_test',
                                data = rmdat_f()) %>%
      addPolylines(
        color = 'orange',
        weight = 5,
        label = ~paste0('Mortality event ',related_mortality,', ',road_surface,' ',road_class),
        # layerId = 'roads',
        group = 'Roads',
        data = roads_in_100m_f()) %>%
      addLegend(pal = mypal,
                values = ~CLASS_NAME,
                title = 'LEGEND',
                data = rmdat_f(),
                layerId = 'legend')
  })
}

## Look for just paved (gravel too?) roads within whichever shape has been clicked.
## Priorities are: paved 4 lane, paved 2 lane, then gravel.

## Add in mortality events from the layers that Sultana queried.

# Run the application
shinyApp(ui = ui, server = server)
