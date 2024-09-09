
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

  # Choose zoom level at which to show parks / roads / culverts
  # on leaflet map
  zoom_reveal_factor = 8

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
  # species_vector = stringr::str_to_title(species_vector[order(species_vector)])

  # ii.
  output$spec_sel_UI = renderUI({
    if(input$want_species_sel_UI == F) return(NULL)
    shinyWidgets::pickerInput(
      inputId = "species_selector",
      label = "Species Selector",
      choices = species_vector,
      selected = species_vector,
      options = list(
        `selected-text-format` = "count > 3",
        `actions-box` = TRUE),
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

  # Test - add jitter to rmdat #
  rmdat = sf::st_jitter(rmdat, factor = 0.0001)

  # Reactive version of dataset.
  rmdat_f = reactive({
    req(input$var_to_color)

    dat_intermediate = rmdat

    # # Spatial match with whatever Province shapes user has chosen.
    # dat_intermediate = dat_intermediate %>%
    #   st_join(st_transform(map_shapes(), 4326), st_intersects)

    # Filter by data source.
    dat_intermediate = dat_intermediate |>
      filter(data_source %in% input$source_filter)

    if(input$want_species_sel_UI){
      dat_intermediate = dat_intermediate %>%
        filter(common_name %in% input$species_selector)
    }

    if(input$var_to_color == 'dat_s'){

      dat_intermediate = dat_intermediate |>
        mutate(color_col = data_source)
      # unique_colours = sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)], length(unique(rmdat$data_source)))
      #
      # dat_intermediate = dat_intermediate |>
      #   mutate(col_for_leaf = case_when(
      #     data_source == unique(rmdat$data_source)[1] ~ unique_colours[1],
      #     data_source == unique(rmdat$data_source)[2] ~ unique_colours[2],
      #     data_source == unique(rmdat$data_source)[3] ~ unique_colours[3],
      #     data_source == unique(rmdat$data_source)[4] ~ unique_colours[4]
      #   ))
    }
    if(input$var_to_color == 'species'){

    #   unique_sp = unique(rmdat$common_name)[order(unique(rmdat$common_name))]
    #   unique_sp = unique_sp[!is.na(unique_sp)]
    #
    #   unique_colours = sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)], length(unique_sp))
    #
    #   sp_col_tbl = tibble(
    #     common_name = unique_sp,
    #     col_for_leaf = unique_colours
    #   )
    #
    #   dat_intermediate = dat_intermediate |>
      #     left_join(sp_col_tbl)

      dat_intermediate = dat_intermediate |>
        mutate(color_col = common_name)
    }

    # dat_intermediate = sf::st_jitter(dat_intermediate)

    return(dat_intermediate)
  })

  output$dat_dt = DT::renderDT({
    rmdat_f() |>
      dplyr::select(incidental, species_code, common_name, scientific_name,date_time,data_source)
  })

  my_pal = reactive({
    if(input$var_to_color == 'dat_s'){
      the_pal = leaflet::colorFactor(palette = 'Spectral',
                                     domain = unique(rmdat$data_source))
    } else {
      the_pal = leaflet::colorFactor(palette = 'Spectral',
                                     domain = unique(rmdat$common_name))
    }
    the_pal
  })

  # Make leaflet
  output$mortmap = initialize_leaflet_map()

  observe({

    # # Need to collapse data file
    # mutate(species_mort_alive = paste0(species,", mort: ",mort_number,", alive: ",alive_number, collapse = '; ')) |>
    #   dplyr::select(-species,-mort_number,-alive_number)

    update_leaflet_map('mortmap',
                       map_shapes(),
                       # input$var_to_color,
                       my_pal(),
                       rmdat_f())
  })
}

shinyApp(ui = ui, server = server)
