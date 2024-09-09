library(shiny)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(sf)
library(bslib)
library(bcdata)
library(plotly)

# Note: idea for a static image while the leaflet map loads is taken
# from here:
# https://community.rstudio.com/t/show-leaflet-spinner-when-rendering-slow-leaflet-map-in-shiny/57896/4

# Define UI for application that draws a histogram
ui <- page_sidebar(
  tags$head(
    tags$style(
      ".map-container {
          height: 100%;
          width: 100%;
          position: relative;
        }",
      ".map-loading {
          position: absolute;
          display: fixed;
          justify-content: center;
          align-items: center;
          width: 100%;
          height: 1000px;
          background-image: url('map_loading_screen_small_grey.png');
          background-repeat: no-repeat;
          text-align: center;
        }"
    )
  ),
  title = 'Herpetofauna Road Mortality Spatial Tool',
  sidebar = sidebar(
    width = '30%',
    # checkboxGroupInput(
    #   inputId = 'group_selector',
    #   label = 'Classes to Include',
    #   inline = T,
    #   choices = c(NULL),
    #   selected = c(NULL)
    # ),
    checkboxInput(inputId = 'want_species_sel_UI',
                  label = "Filter by Species?",
                  value = F),
    uiOutput('spec_sel_UI'),
    shinyWidgets::pickerInput(
      inputId = "source_filter",
      label = "Data Sources to include",
      choices = c("Leigh-Anne",
                  "Herp 2022 Sheet" = 'Herpetofauna_Road_Mortality_ExcelSheet_2022',
                  "BC Data Catalogue - SPI",
                  "iNaturalist"),
      selected = c("Leigh-Anne",
                   "Herp 2022 Sheet" = 'Herpetofauna_Road_Mortality_ExcelSheet_2022',
                   "BC Data Catalogue - SPI",
                   "iNaturalist"),
      options = list(
        `selected-text-format` = "count > 2",
        `actions-box` = TRUE),
      multiple = TRUE
    ),
    selectizeInput(
      inputId = 'choose_spatial_containers',
      label = 'Select Spatial Divisions',
      choices = c('Natural Resource Regions' = 'nr_regions',
                  'Natural Resource Districts' = 'nr_districts',
                  'Ecoprovinces' = 'ecoprovs',
                  'Ecoregions' = 'ecoregions',
                  'Ecosections' = 'ecosects'),
      selected = 'nr_regions'
    ),
    selectizeInput('var_to_color',
                   "Color Points by:",
                   choices = c("Data Source" = 'dat_s',
                               "Species" = 'species')),
    card(
      layout_column_wrap(
        1/2,
        card(
          h6("Total Events"),
          div(
            textOutput('total_records'),
            style = 'margin-top:-1rem;'
          ),
          style = 'text-align:center;background:bisque;'
        ),
        card(
          h6("Distinct Species"),
          div(
            textOutput('number_dist_species'),
            style = 'margin-top:-1rem;'
          ),
          style = 'text-align:center;background:cadetblue;'
        )
      )
    ),
    plotOutput('data_time_hist', height = '250px')
  ),
  tags$div(
    class = "map-container",
    tags$div(
      id = "leafletBusy",
      class = "map-loading",
      tags$p("Loading map... please wait around 20 seconds",
             style = 'margin-top:8rem;')
    ),
      leafletOutput("mortmap", height = '500px'),
      DT::DTOutput('dat_dt', height = '200px')
  )
)
