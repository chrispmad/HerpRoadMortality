
source('utils/UI.R')

# Define server logic
server <- function(input, output, session) {

  # Functions and whatnot from the /utils folder:
  # 1. Make (javascript?) functions for loading screen for map.
  source('utils/loading_screen_functions.R')
  # 2. Read in data from /www folder.
  source('utils/data_loader.R')$value
  # 3. Function to make histogram of road mortality events by year.
  source('utils/make_histogram_plot.R')
  # 4. Function to start our leaflet map.
  source('utils/initialize_leaflet_map.R')
  # 5. Function to update leaflet map.
  source('utils/update_leaflet_map.R')

  # Put all 'container shapes' into a list; then
  # the user can choose from this list.
  container_shape_l = list(nr_regions,
                           nr_districts,
                           ecoprovs,
                           ecosects,
                           ecoregions) |>
    set_names(c("nr_regions","nr_districts","ecoprovs","ecosects","ecoregions"))

  # Render UI elements that depend on columns from the dataset(s).
  # i.
  species_vector = unique(rmdat$common_name)
  species_vector = stringr::str_to_title(species_vector[order(species_vector)])

  # ii.
  output$spec_sel_UI = renderUI({
    if(input$want_species_sel_UI == F) return(NULL)
    shinyWidgets::pickerInput(
      inputId = "species_selector",
      label = "Species Selector",
      choices = species_vector,
      selected = species_vector,
      options = list(
        `selected-text-format` = "count > 3"),
      multiple = TRUE
    )
  })

  # Render summary widgets
  # i. total number of road mortality events.
  output$total_records = renderText({
    nrow(rmdat_f())
  })

  # ii. number of unique species with road mortality events.
  output$number_dist_species = renderText({
    length(unique(rmdat_f()$common_name))
  })

  # iii. Histogram of # records by year
  output$data_time_hist = make_histogram_plot(dat = rmdat_f())

  # Select which shapes (e.g. regions/districts) to map
  map_shapes = reactive({
    container_shape_l[[input$choose_spatial_containers]]
  })

  # Reactive version of dataset.
  rmdat_f = reactive({
    req(map_shapes())#, click_shape())
    dat_intermediate = rmdat #%>%
      # filter(CLASS_NAME %in% input$group_selector)

    # Spatial match with whatever Province shapes user has chosen.
    dat_intermediate = dat_intermediate %>%
      st_join(st_transform(map_shapes(), 4326), st_intersects)

    if(input$want_species_sel_UI){
      dat_intermediate = dat_intermediate %>%
        filter(common_name %in% input$species_selector)
    }
    return(dat_intermediate)
  })

  # Make leaflet
  output$mortmap = initialize_leaflet_map(parks)

  observe({
    update_leaflet_map('mortmap',
                       map_shapes(),
                       rmdat_f())
  })
}

shinyApp(ui = ui, server = server)
