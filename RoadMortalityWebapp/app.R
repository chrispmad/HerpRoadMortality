library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(bs4Dash)
library(bslib)
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
    title = "Map Controls",
    width = '30%',
    bs4Dash::bs4Accordion(
      id = 'my_acc',
      bs4Dash::bs4AccordionItem(
        collapsed = F,
        status = 'navy',
        title = 'Filtering Options',
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
        )
        # DT::DTOutput('test')
      ),
      bs4Dash::bs4AccordionItem(
        # id = 'summ_widgs',
        title = 'Summary Widgets',
        status = 'navy',
        valueBoxOutput('total_records',
                       width = 12),
        valueBoxOutput('number_dist_species',
                       width = 12),
        plotOutput('data_time_hist', height = '250px')
      )
    )
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
    st_transform(crs = 4326)
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
             gradient = F,
             icon = icon('check'))
  })

  # ii.
  output$number_dist_species = renderValueBox({
    valueBox(value = length(unique(rmdat_f()$SPECIES_EN)),
             subtitle = 'Distinct Species',
             width = 12,
             col = 'warning',
             icon = icon('x'))
    })

  # iii. Histogram of # records by year
  output$data_time_hist = renderPlot({
    dat_histogram = rmdat_f() %>%
      ggplot() +
      geom_histogram(aes(OBSERVAT_1),
                     fill = 'white') +
      labs(y = 'Number of Records',
           x = 'Year') +
      scale_y_continuous(breaks = scales::breaks_pretty()) +
      coord_flip() +
      theme_dark() +
      theme(axis.text = element_text(colour = 'white',size = 13),
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

  rmdat_f = reactive({
    dat_intermediate = rmdat %>%
      filter(CLASS_NAME %in% input$group_selector)
    if(input$want_species_sel_UI){
      dat_intermediate = dat_intermediate %>%
        filter(SPECIES_EN %in% input$species_selector)
    }
    return(dat_intermediate)
  })
  # Make reactive versions of data.


  # Make leaflet
  output$mortmap = renderLeaflet({
    leaflet() %>%
      addTiles(group = 'Streets') %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
      addProviderTiles(providers$CartoDB,  group = "CartoDB") %>%
      # addProviderTiles("Esri.WorldImagery",group = "Sat") %>%
      # addPolygons(data = nr_regions,
      #             label = ~REGION_NAME,
      #             col = 'grey',
      #             weight = 2,
      #             group = 'Regions') %>%
      envreportutils::add_bc_home_button() %>%
      envreportutils::set_bc_view(zoom = 5) %>%
      addLayersControl(baseGroups = c("CartoDB","Streets","Terrain","Satellite"),
                       overlayGroups = c("Regions"),
                       options = layersControlOptions(collapsed = F),
                       position = 'bottomright')
  })

  mypal = colorFactor(palette = 'Spectral',
                domain = rmdat$CLASS_NAME)

  output$test = DT::renderDT({
    map_shapes()
  })

  observe({
    leafletProxy("mortmap") %>%
      removeControl("legend") %>%
      leaflet::clearMarkers() %>%
      leaflet::clearShapes() %>%
      addPolygons(
        data = map_shapes(),
        label = ~shape_name,
        col = 'grey',
        weight = 2,
        group = 'MapShapes') %>%
      leaflet::addCircleMarkers(#layerId = "mort_points",
                                color = ~mypal(CLASS_NAME),
                                radius = 3,
                                weight = 10,
                                opacity = 0.8,
                                label = ~paste0(SPECIES_EN, " (",SCIENTIFIC,",",Date,"): ",SIGN_OR__1,", ",OBSERVAT_5),
                                data = rmdat_f()) %>%
      addLegend(pal = mypal,
                values = ~CLASS_NAME,
                title = 'LEGEND',
                data = rmdat_f(),
                layerId = 'legend')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
