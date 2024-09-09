library(tidyverse)
library(sf)
library(bcdata)
library(bcmaps)

# Set up directories to store data: /data, /www

if(!dir.exists('data')) dir.create('data')
if(!dir.exists('RoadMortalityWebapp/www')) dir.create('RoadMortalityWebapp/www')

refresh_shapes = F
update_roads_culverts = F

#############
# Functions #
#############
common_name_corrector = function(dat, spec_col_name) {
  dat |>
    mutate(!!as.name(spec_col_name) := stringr::str_squish(stringr::str_to_title(!!as.name(spec_col_name)))) |>
    mutate(!!as.name(spec_col_name) := case_when(
      !!as.name(spec_col_name) == 'Northern Pacific Tree Frog' ~ "Pacific Treefrog",
      !!as.name(spec_col_name) == 'Northern Pacific Treefrog' ~ "Pacific Treefrog",
      !!as.name(spec_col_name) == 'Terrestrial Gartersnake' ~ 'Western Terrestrial Gartersnake',
      !!as.name(spec_col_name) == 'Unknown Amphibian' ~ 'Unknown amphibian',
      # !!as.name(spec_col_name) == 'Blotched Tiger Salamander' ~ 'Western Tiger Salamander (Blotched Tiger Salamander)',
      !!as.name(spec_col_name) == 'Blotched Tiger Salamander' ~ 'Western Tiger Salamander',
      !!as.name(spec_col_name) == 'Desert Night Snake deserticola' ~ "Desert Nightsnake",
      !!as.name(spec_col_name) %in% c('Gopher Snake, Deserticola Subspecies', 'Gophersnake, Deserticola Subspecies', 'Gopher Snake') ~ "Great Basin Gophersnake",
      !!as.name(spec_col_name) == 'Harmless Snakes' ~ "Unknown Snakes",
      !!as.name(spec_col_name) == 'Painted Turtle bellii' ~ 'Western Painted Turtle',
      !!as.name(spec_col_name) == 'Painted Turtle' ~ 'Western Painted Turtle',
      !!as.name(spec_col_name) == 'reptiles' ~ "Unknown Reptiles",
      !!as.name(spec_col_name) == 'Snakes and Lizards' ~ "Unknown Reptiles",
      !!as.name(spec_col_name) == 'Common Wall Lizard' ~ "Wall Lizard",
      !!as.name(spec_col_name) == 'Roughskin Newt' ~ "Rough-skinned Newt",
      !!as.name(spec_col_name) == 'North American Racer' ~ "Western Yellow-bellied Racer",
      !!as.name(spec_col_name) == 'Great Basin Gopher Snake' ~ "Great Basin Gophersnake",
      !!as.name(spec_col_name) == 'Common Garter Snake' ~ "Common Gartersnake",
      !!as.name(spec_col_name) %in% c('Northwestern Garter Snake','Northwestern Garter snake') ~ 'Northwestern Gartersnake',
      !!as.name(spec_col_name) == 'Western Terrestrial Garter Snake' ~ 'Western Terrestrial Gartersnake',
      TRUE ~ !!as.name(spec_col_name)
    ))
}

sci_name_corrector = function(dat, sci_col_name) {
  dat |>
    mutate(!!as.name(sci_col_name) := case_when(
      !!as.name(sci_col_name) == 'Chrysemys picta bellii' ~ 'Chrysemys picta',
      !!as.name(sci_col_name) %in% c('Coluber constrictor mormon',
                        'Coluber mormon') ~ 'Coluber constrictor',
      !!as.name(sci_col_name) == 'Crotalus oreganus oreganus' ~ 'Crotalus oreganus',
      !!as.name(sci_col_name) == 'Hypsiglena chlorophaea deserticola' ~ 'Hypsiglena chlorophaea',
      !!as.name(sci_col_name) %in% c('Pituophis catenifer',
                        'Pituophis melanoleucus') ~ 'Pituophis catenifer deserticola',
      T ~ !!as.name(sci_col_name)
    ))
}


#############
# Load data #
#############

# Spatial layers from {bcdata}

if(refresh_shapes == T){
# i. Different regions/districts/boundaries within Province
bcmaps::nr_regions() |>
  st_simplify(dTolerance = 200)  |>
  rename(shape_name = REGION_NAME) |>
  dplyr::select(shape_name) |>
  sf::st_transform(4326) |>
  write_sf('RoadMortalityWebapp/www/nr_regions.gpkg')

bcmaps::nr_districts() |>
  st_simplify(dTolerance = 200) |>
  rename(shape_name = DISTRICT_NAME) |>
  dplyr::select(shape_name) |>
  sf::st_transform(4326) |>
  write_sf('RoadMortalityWebapp/www/nr_districts.gpkg')

bcmaps::ecoprovinces() |>
  st_simplify(dTolerance = 200) |>
  rename(shape_name = ECOPROVINCE_NAME) |>
  mutate(shape_name = stringr::str_to_title(shape_name)) |>
  dplyr::select(shape_name) |>
  sf::st_transform(4326) |>
  write_sf('RoadMortalityWebapp/www/ecoprovinces.gpkg')

bcmaps::ecoregions() |>
  st_simplify(dTolerance = 200) |>
  rename(shape_name = ECOREGION_NAME) |>
  mutate(shape_name = stringr::str_to_title(shape_name)) |>
  dplyr::select(shape_name) |>
  sf::st_transform(4326) |>
  write_sf('RoadMortalityWebapp/www/ecoregions.gpkg')

bcmaps::ecosections() |>
  st_simplify(dTolerance = 200) |>
  rename(shape_name = ECOSECTION_NAME) |>
  dplyr::select(shape_name) |>
  sf::st_transform(4326) |>
  write_sf('RoadMortalityWebapp/www/ecosections.gpkg')
}

# Species of interest
species_of_int = openxlsx::read.xlsx('Species of Interest.xlsx')
sci_names = str_squish(species_of_int$Scientific.Name)
com_names = c(str_squish(species_of_int$Common.Name),
              "Racer","Rubber Boa","Unknown amphibian",
              "Great Basin Gophersnake",
              "Unknown gartersnake","Unknown snake",
              "Unknown reptile")

# Common Name Corrector Lookup Table
name_corrector = openxlsx::read.xlsx("./data/common_name_correction_lookup_table.xlsx") |>
  dplyr::select(common_name, scientific_name, new_name) |>
  dplyr::distinct() |>
  dplyr::arrange(common_name) |>
  dplyr::filter(!is.na(new_name)) |>
  filter(!duplicated(scientific_name))

# 2022 Excel Sheet
# herp_road_2022 = openxlsx::read.xlsx('./data/OccurrenceData/Herpetofauna_Road_Mortality_ExcelSheet_2022.xlsx')
# openxlsx::write.xlsx(herp_road_2022, './data/OccurrenceData/Herpetofauna_Road_Mortality_ExcelSheet_2022.xlsx')
herp_road_2022 = readxl::read_xlsx('./data/OccurrenceData/Herpetofauna_Road_Mortality_ExcelSheet_2022.xlsx')

# Pivot longer all the species mortality code columns.
all_p_together = herp_road_2022 |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(cols = starts_with("Species_Code_Mortality"),
               names_to  = 'species_variable', values_to = "species") |>
  pivot_longer(cols = starts_with("#_Mort"),
               names_to  = 'mort_variable', values_to = "mort_number") |>
  pivot_longer(cols = starts_with("#_Al"),
               names_to  = 'alive_variable', values_to = "alive_number") |>
  filter(!is.na(species)) |>
  filter(as.numeric(str_extract(species_variable,'[0-9]+$')) + 1 == as.numeric(str_extract(mort_variable,'[0-9]+$')),
         as.numeric(str_extract(species_variable,'[0-9]+$')) + 2 == as.numeric(str_extract(alive_variable,'[0-9]+$'))) |>
  dplyr::select(-tessellate,
                -ends_with('_variable')) |>
  filter(!is.na(mort_number) | !is.na(alive_number)) |>
  group_by(x, y, Location, Year) |>
  mutate(species = replace(species, species == 'THIS', 'THSI'),
         species = replace(species, species == 'ANBU', 'ANBO')) |>
  mutate(mort_number = replace_na(mort_number, '0'),
         alive_number = replace_na(alive_number, '0')) |>
  # mutate(species_mort_alive = paste0(species,", mort: ",mort_number,", alive: ",alive_number, collapse = '; ')) |>
  # dplyr::select(-species,-mort_number,-alive_number) |>
  distinct() |>
  ungroup()

# SPI New Search Herps Only xlsx from Megand Winand and Alex
spi_new_search = openxlsx::read.xlsx('data/OccurrenceData/SPI_New_Search_HerpsOnly.xlsx')

# Mortality Events data
# # i. KMZ / KML / GDB files from Leigh Anne.
# occ_data_zipped = list.files('data/OccurrenceData/',full.names = T)
#
# kmz_layers = occ_data_zipped[str_detect(occ_data_zipped,'\\.kmz')] %>%
#   as.list() %>%
#   map( ~ read_sf(unzip(.x)))
# # list elements 1 and 2 are identical. 3rd element empty. 4th is usable. 5th has no written location.
# # No need to use the kmz layers.
#
# kml_layers = occ_data_zipped[str_detect(occ_data_zipped,'\\.kml')] %>%
#   as.list() %>%
#   map( ~ read_sf(.x)) %>%
#   bind_rows()
# # KML layer is just fourth element of KMZ, perfect.
#
# kml_layers = kml_layers %>%
#   mutate(Description = str_replace(Description, 'Locatoin','Location'))

# "Leigh-Anne" data, i.e. the first SPI download from Sultana.
herp_mortality_gdb = read_sf('data/OccurrenceData/SpatialFiles_AmphibRepti_mortalities.gdb/') |>
  dplyr::select( -OBJECTID ) |>
  dplyr::rename(GEOMETRY = Shape)

# Second gbd from Sultana Majid, Feb 2023.
herp_mortality_gdb2 = read_sf('data/OccurrenceData/SpatialFiles_AmphibReptile_Morts_Feb2023.gdb/')

#Winton 2015
winton_15 = readxl::read_excel('data/OccurrenceData/winton 2015 Road Mortalities and Encounters.xlsx')
  # Keep only rows that match our species of interest.

winton_15 = winton_15 |>
  dplyr::filter(Species %in% c(species_of_int$Code, species_of_int$Scientific.Name, species_of_int$Common.Name)) |>
  dplyr::rename(Code = Species) |>
  left_join(species_of_int)

# # We could make new columns based on the age class and sex of the little guys/gals.
# winton_15 |>
#   dplyr::mutate(adult_male )

#Winton 2016
winton_16 = readxl::read_excel('data/OccurrenceData/winton 2016 Road Mortalities and Encounters.xlsx')

winton_16 = winton_16 |>
  dplyr::filter(Species %in% c(species_of_int$Code, species_of_int$Scientific.Name, species_of_int$Common.Name)) |>
  dplyr::rename(Code = Species) |>
  left_join(species_of_int)

herp_mortality_gdb = herp_mortality_gdb |>
  set_names(names(herp_mortality_gdb2)) |>
  bind_rows(herp_mortality_gdb2) |>
  distinct()

rm(herp_mortality_gdb2)

# Use Megan's common_name corrector lookup table on all the data.
herp_mortality_gdb = herp_mortality_gdb |>
  dplyr::rename(common_name = SPECIES_ENGLISH_NAME,
                scientific_name = SCIENTIFIC_NAME) |>
  left_join(name_corrector |>
              dplyr::select(common_name, scientific_name, new_name)) |>
  mutate(common_name = ifelse(!is.na(new_name), new_name, common_name))

la_sp = unique(herp_mortality_gdb$common_name)
la_sp[str_detect(la_sp,'Racer')]

herp_mortality_gdb = herp_mortality_gdb |>
  common_name_corrector('common_name') |>
  sci_name_corrector('scientific_name')

# Match Leigh Anne's data to the excel file of species of interest that Megan Winand gave me.
# write_sf(kml_layers, 'RoadMortalityWebapp/www/KML_roadmort.gpkg')

# 2. iNaturalist; NOTE: this is a manual download (ugh)
#    Specifically, from this project: https://inaturalist.ca/projects/canadian-amphibians-reptiles-on-roads
#    with quality being set to Research Grade.
#    Here is the full URL for the query:
#    https://inaturalist.ca/observations/export?quality_grade=research&identifications=most_agree&place_id=7085&identifications=any&projects%5B%5D=canadian-amphibians-reptiles-on-roads&term_id=17&term_value_id=19
inat_data = list.files(path = 'data/OccurrenceData/',
           pattern = '^canadian-amphibians-.*.csv',
           full.names = T) |>
  lapply(read_csv) |>
  bind_rows()

# Some corrections by hand that Megan told me about.
inat_data = inat_data |>
  mutate(common_name = ifelse(id %in% c(50073482,168881303), 'Western Painted Turtles', common_name)) |>
  mutate(scientific_name = ifelse(id %in% c(50073482,168881303), 'Chrysemys picta bellii', scientific_name)) |>
  mutate(quality_grade = ifelse(id %in% c(50073482,168881303), 'research', quality_grade))

inat_data = inat_data |>
  filter(scientific_name %in% sci_names | common_name %in% com_names) |>
  mutate(common_name = replace(common_name, common_name == 'Barred Tiger Salamander', 'Western Tiger Salamander'))

# Adjust format to match 'herp_mortality_data.gpkg'

# 4. SPI - This is a query for data from bc data catalogue's live layer.
spi_dat = bcdc_query_geodata('wildlife-species-inventory-incidental-observations-non-secured') |>
  filter(SCIENTIFIC_NAME %in% sci_names | SPECIES_ENGLISH_NAME %in% com_names) |>
  # dplyr::filter(PROJECT_ID %in% spi_new_search$SPI_PROJECT_ID) |>
  collect()

spi_dat_on_list = spi_dat |>
  dplyr::filter(PROJECT_ID %in% spi_new_search$SPI_PROJECT_ID)

# Just keep records with some mention of dead/carcass/mortality in the comments field,
# OR that is actually classified as 'Carcass (not an activity)' in the activity field.!
spi_mort_data = spi_dat_on_list |>
  filter(str_detect(OBSERVATION_COMMENTS, "([d,D]ead|[c,C]arcass|[m,M]ort(ality)?)") |
           SIGN_OR_ACTIVITY == 'Carcass (not an activity)')

# Correct common names using our name corrector function.
spi_mort_data = spi_mort_data |>
  common_name_corrector('SPECIES_ENGLISH_NAME')

spi_mort_data |>
  dplyr::select(starts_with("PROJECT")) |>
  st_drop_geometry() |>
  write_csv('output/SPI_project_numbers.csv')
# I've sent the above CSV file to Megan.

# 4. Regional Bios etc.

# Spatial files from BCG Warehouse and simplify the geometries before we visualize.
bcdata_list = bcdc_list()

# i. BC Parks/protected areas
if(!file.exists('data/parks_simplified.gpkg')){
  if(!file.exists('data/parks_fullres.gpkg')){
    parks = bcdc_query_geodata('terrestrial-protected-areas-representation-by-ecosection-parc-') %>%
      collect()
    write_sf(parks,'data/parks_fullres.gpkg')
  }else{
    parks = read_sf('data/parks_fullres.gpkg')
  }
  parks_simple = st_simplify(parks, dTolerance = 200)
  parks_simple = sf::st_transform(parks_simple)
  ggplot() + geom_sf(data = parks_simple)
  write_sf(parks_simple, 'RoadMortalityWebapp/www/parks_simplified.gpkg')
}

# Combine various datasets and change the column names. This section makes a bunch
# of data objects that are called "..._for_join"; at the end of the section,
# all of these objects are joined together into one main data object.

herp_mortality_gdb_for_join = herp_mortality_gdb |>
  reframe(
    incidental = INCIDENTAL_OBSERVATION_ID,
    species_code = SPECIES_CODE,
    common_name = common_name,
    scientific_name = scientific_name,
    BC_see_spec = BCSEE_SPECIES_CODE_URL,
    date_time = OBSERVATION_DATE,
    year = OBSERVATION_YEAR,
    month = OBSERVATION_MONTH,
    day = OBSERVATION_DAY,
    data_source = 'Leigh-Anne',
    proj_name = PROJECT_NAME,
    proj_id = PROJECT_ID,
    proj_web = PROJECT_WEB_PAGE,
    utm_zone = UTM_ZONE,
    easting = UTM_EASTING,
    northing = UTM_NORTHING,
    lat = LATITUDE,
    lng = LONGITUDE,
    adult_male = ADULT_MALES,
    adult_female = ADULT_FEMALES,
    juv_male = JUVENILE_MALES,
    juv_female = JUVENILE_FEMALES,
    sign_or_activity = SIGN_OR_ACTIVITY,
    comments = OBSERVATION_COMMENTS,
    status = PROVINCIAL_LIST,
    cosewic = COSEWIC_CD,
    class_name = CLASS_NAME,
    geometry = GEOMETRY
  )
herp_mortality_gdb_for_join = st_set_geometry(herp_mortality_gdb_for_join, herp_mortality_gdb_for_join$geometry)
herp_mortality_gdb_for_join = st_transform(herp_mortality_gdb_for_join,crs = 4326)



herp_road_2022 |>
  mutate(mortality_number = as.numeric(mortality_number),
         alive_number = as.numeric(alive_number)) |>
  mutate(mortality_number = replace_na(mortality_number, 0),
         alive_number = replace_na(alive_number, 0)) |>
  group_by(across(-contains('_number'))) |>
  summarise(mortality_number = sum(mortality_number),
            alive_number = sum(alive_number)) |>
  filter(Location == 'Mike Lake') |>
  View()



herp_road_2022_for_join = all_p_together |>
  filter(!is.na(x)) |>
  # group_by(x, y, Location) |>
  # summarise(across(everything())) |>
  st_as_sf(coords = c("x","y"), crs = 4326) |>
  reframe(
    # species_code = species,
    common_name = species,
    year = as.numeric(Year),
    data_source = 'Herpetofauna_Road_Mortality_ExcelSheet_2022',
    proj_name = Report_Title,
    comments = Comments,
    class_name = Taxa,
    mort_number,
    alive_number,
    geometry = geometry
  ) |>
  dplyr::rename(Code = common_name) |>
  left_join(species_of_int) |>
  dplyr::rename(common_name = Common.Name,
                scientific_name = Scientific.Name)

herp_road_2022_for_join = st_set_geometry(herp_road_2022_for_join, herp_road_2022_for_join$geometry)
herp_road_2022_for_join = st_transform(herp_road_2022_for_join,crs = 4326)

winton_15_data_for_join = winton_15 |>
  mutate(Easting = as.numeric(Easting),
         Northing = as.numeric(Northing)) |>
  filter(!is.na(Easting),
         !is.na(Northing)) |>
  st_as_sf(coords = c("Easting","Northing"),
           crs = 32611) |>
  reframe(
    species_code = Code,
    common_name = Common.Name,
    date_time = openxlsx::convertToDate(Date),
    scientific_name = Scientific.Name,
    data_source = 'Winton_2015',
    proj_name = 'Winton?',
    comments = Comments,
    class_name = 'Unknown',
    geometry = geometry
    ) |>
  mutate(year = lubridate::year(date_time),
         month = lubridate::month(date_time),
         day = lubridate::day(date_time)
         ) |>
  mutate(date_time = paste0(date_time,' 00:00:01')) |>
  mutate(date_time = as_datetime(date_time))

winton_15_data_for_join = st_set_geometry(winton_15_data_for_join, winton_15_data_for_join$geometry)
winton_15_data_for_join = st_transform(winton_15_data_for_join,crs = 4326) |>
  mutate(lat = st_coordinates(geometry)[,2],
         lng = st_coordinates(geometry)[,1])


winton_16_data_for_join = winton_16 |>
  mutate(Easting = as.numeric(Easting),
         Northing = as.numeric(Northing)) |>
  filter(!is.na(Easting),
         !is.na(Northing)) |>
  st_as_sf(coords = c("Easting","Northing"),
           crs = 32611) |>
  reframe(
    species_code = Code,
    common_name = Common.Name,
    scientific_name = Scientific.Name,
    date_time = `Date and time`,
    data_source = 'Winton_2015',
    proj_name = 'Winton?',
    comments = Comments,
    class_name = 'Unknown',
    geometry = geometry
  ) |>
  mutate(year = lubridate::year(date_time),
         month = lubridate::month(date_time),
         day = lubridate::day(date_time)
  )

winton_16_data_for_join = st_set_geometry(winton_16_data_for_join, winton_16_data_for_join$geometry)

winton_16_data_for_join = winton_16_data_for_join |> st_transform(crs = 4326)  |>
  mutate(lat = st_coordinates(geometry)[,2],
         lng = st_coordinates(geometry)[,1])


inat_data_for_join = inat_data |>
  reframe(
    common_name,
    scientific_name,
    date_time = lubridate::ymd_hms(time_observed_at),
    inat_credit = paste0(user_name,', ',license, ', ', url),
    data_source = 'iNaturalist',
    proj_name = 'Canadian Amphibians & Reptiles on Roads',
    proj_id = 94083,
    lat = latitude,
    lng = longitude,
    comments = description,
    proj_web = 'https://inaturalist.ca/projects/canadian-amphibians-reptiles-on-roads'
  ) |>
  mutate(
    year = lubridate::year(date_time),
    month = lubridate::month(date_time),
    day = lubridate::day(date_time),
    lat2 = lat,
    lng2 = lng
  ) |>
  st_as_sf(coords = c("lng2","lat2"), crs = 4326)

spi_mort_data_for_join = spi_mort_data |>
  reframe(incidental = INCIDENTAL_OBSERVATION_ID,
          species_code = SPECIES_CODE,
          common_name = SPECIES_ENGLISH_NAME,
          scientific_name = SCIENTIFIC_NAME,
          BC_see_spec = BCSEE_SPECIES_CODE_URL,
          date_time = OBSERVATION_DATE,
          year = OBSERVATION_YEAR,
          month = OBSERVATION_MONTH,
          day = OBSERVATION_DAY,
          data_source = 'BC Data Catalogue - SPI',
          proj_name = PROJECT_NAME,
          proj_id = PROJECT_ID,
          proj_web = PROJECT_WEB_PAGE,
          utm_zone = UTM_ZONE,
          easting = UTM_EASTING,
          northing = UTM_NORTHING,
          lat = LATITUDE,
          lng = LONGITUDE,
          adult_male = as.numeric(ADULT_MALES),
          adult_female = as.numeric(ADULT_FEMALES),
          juv_male = as.numeric(JUVENILE_MALES),
          juv_female = as.numeric(JUVENILE_FEMALES),
          sign_or_activity = SIGN_OR_ACTIVITY,
          comments = OBSERVATION_COMMENTS,
          status = PROVINCIAL_RANK,
          cosewic = COSEWIC_CD,
          class_name = CLASS_NAME,
          geometry)

spi_mort_data_for_join = st_set_geometry(spi_mort_data_for_join, spi_mort_data_for_join$geometry)
spi_mort_data_for_join = st_transform(spi_mort_data_for_join,crs = 4326)

all_mort_data = herp_mortality_gdb_for_join |>
  bind_rows(herp_road_2022_for_join) |>
  bind_rows(spi_mort_data_for_join) |>
  bind_rows(inat_data_for_join) |>
  bind_rows(winton_15_data_for_join) |>
  bind_rows(winton_16_data_for_join) |>
  distinct() |>
  common_name_corrector('common_name') |>
  sci_name_corrector('scientific_name')

# Add row index.
all_mort_data = all_mort_data |>
  mutate(common_name = stringr::str_squish(stringr::str_to_title(common_name))) |>
  mutate(common_name = stringr::str_squish(stringr::str_to_sentence(common_name))) |>
  common_name_corrector('common_name') |>
  sci_name_corrector('scientific_name') |>
  mutate(row_id = row_number()) |>
  dplyr::select(row_id, everything())

write_sf(all_mort_data,
         'RoadMortalityWebapp/www/herp_mortality_data.gpkg')

if(update_roads_culverts){
# I've commented out the section below... I haven't removed the data
# from the app's /www folder yet, but probably will, as the code below
# only accesses and simplifies geometries for a piddly number of road mortality events;
# if we try to do the following for all the iNaturalist / SPI data, I think it
# might be too big a hassle / the dataset(s) might be too large!

# ii. Culverts. Note: Leigh Anne: "I also learned from Karina where the 'best'
#     culvert spatial file is. It is: BCGW in something called 'criss mapping'.
#     I may have this word TOTALLY misspelled. There is a file that refers to culvert
#     mapping."
bcdata_list[str_detect(bcdata_list,'culvert')]

# bcdc_query_geodata('ministry-of-transportation-mot-culverts') |>
#   filter(DWITHIN(all_mort_data, 5, 'kilometers'))

# iii. Roads (highways and gravel roads; FSR if available)
bcdata_list[str_detect(bcdata_list,'road-atlas')]

# Query the giant roads spatial file by row, to avoid downloading a huge chunk of roads.
# Here, we just cycle through each road mortality incident, buffering 100m around the
# occurrence point of the mortality incident, and downloading any roads within that radius.
road_pieces = list()
culverts = list()

# if(file.exists('./data/road_pieces_backup_1560_successful.gpkg')){
#   road_pieces = sf::read_sf('./data/road_pieces_backup_1560_successful.gpkg') |>
#     dplyr::rename(geometry = geom)
#   culverts = sf::read_sf('./data/culverts_backup_1560_successful.gpkg') |>
#     dplyr::rename(geometry = geom)
# }
for(i in 1:nrow(all_mort_data)){

  print(i)

  this_row = all_mort_data[i,] |> sf::st_transform(3005)

  culvs_i = bcdc_query_geodata('ministry-of-transportation-mot-culverts') |>
    bcdata::filter(DWITHIN(this_row, 0.5, 'kilometers')) |>
    collect() |>
    mutate(across(-geometry, as.character)) |>
    mutate(mort_id = this_row$row_id)

  roads_i = bcdc_query_geodata('digital-road-atlas-dra-master-partially-attributed-roads') |>
    bcdata::filter(DWITHIN(this_row, 0.5, 'kilometers')) |>
    collect() |>
    mutate(across(-geometry, as.character)) |>
    mutate(mort_id = this_row$row_id)

  if(nrow(culvs_i) > 0){
    culverts[[i]] = culvs_i
    # culverts = bind_rows(culverts, culvs_i)
  }
  if(nrow(roads_i) > 0){
    road_pieces[[i]] = roads_i
    # road_pieces = bind_rows(road_pieces, roads_i)
  }
}


# Lost connection to BCG Warehouse! Saving intermediates...

# road_pieces_backup = bind_rows(road_pieces)
# culverts_backup = bind_rows(culverts)

# write_sf(road_pieces_backup, './data/road_pieces_backup_1560_successful.gpkg')
# write_sf(culverts_backup, './data/culverts_backup_1560_successful.gpkg')



culverts_bound = bind_rows(culverts)
roads_bound = bind_rows(road_pieces)

culv_b_d = distinct(culverts_bound)
roads_b_d = distinct(roads_bound)

culv_b_d = sf::st_transform(culv_b_d, 4326)
roads_b_d = sf::st_transform(roads_b_d, 4326)

# sum(st_is_empty(culv_b_d$geometry))

# Clean up columns; there are too many!!
names(culv_b_d)
culv_b_d = culv_b_d |>
  dplyr::select(culvert_id = CULVERT_ID,
                type = CULVERT_TYPE,
                diam = CULVERT_DIAMETER,
                material = CULVERT_MATERIAL,
                inst_date = INSTALLATION_DATE,
                highway_number = HIGHWAY_NUMBER,
                mort_id)

names(roads_b_d)



roads_b_d = roads_b_d |>
  dplyr::select(dra_id = DIGITAL_ROAD_ATLAS_LINE_ID,
                route_number = HIGHWAY_ROUTE_NUMBER,
                length_2D = SEGMENT_LENGTH_2D,
                name_alias = ROAD_NAME_ALIAS1,
                name_alias_id = ROAD_NAME_ALIAS1_ID,
                surface = ROAD_SURFACE,
                class = ROAD_CLASS,
                num_lanes = NUMBER_OF_LANES,
                node_from_id = NODE_FROM_ID,
                node_to_id = NODE_TO_ID,
                data_capt_date = DATA_CAPTURE_DATE,
                mort_id)

# mortality_road_within_100m = herp_mortality_gdb$INCIDENTAL %>%
#   as.list() %>%
#   map(., ~ {
#     tryCatch(
#     expr = bcdc_query_geodata('digital-road-atlas-dra-master-partially-attributed-roads') %>%
#       filter(INTERSECTS(st_buffer(herp_mortality_gdb[herp_mortality_gdb$INCIDENTAL == .x,],dist = 100))) %>%
#       collect() %>%
#       mutate(related_mortality = .x) %>%
#       dplyr::select(related_mortality, DIGITAL_ROAD_ATLAS_LINE_ID, FEATURE_TYPE, SEGMENT_LENGTH_2D, ROAD_SURFACE, ROAD_CLASS, DATA_CAPTURE_DATE) %>%
#       setNames(snakecase::to_snake_case(names(.))) %>%
#       mutate(across(-geometry, as.character)),
#     error = function(e) {
#       #print(paste0("error - no data available for herp mortality incidental number: ", .x))
#       # data.frame(related_mortality = .x, lat = NA, lon = NA) %>%
#       #   st_as_sf(coords = c('lon','lat'), crs = 4326) %>%
#       #   st_transform(crs = 3005)
#     })
#   })
#
# all_mort_roads_within_100m = bind_rows(mortality_road_within_100m)

#Write to disk.
write_sf(roads_b_d,'RoadMortalityWebapp/www/roads_within_500_m.gpkg')
write_sf(culv_b_d,'RoadMortalityWebapp/www/culverts_within_500_m.gpkg')

# write_sf(all_mort_roads_within_100m,'RoadMortalityWebapp/www/roads_within_100m.gpkg')

# roads = read_sf('RoadMortalityWebapp/www/roads_within_100m.gpkg')
#
# ggplot() + geom_sf(data = roads %>%
#   filter(related_mortality == '58234'),
#   aes(col = road_class)) +
#   geom_sf(data = morts %>% filter(INCIDENTAL == '58234'))
#
# ggplot() + geom_sf(data = roads)
#
# # How about getting ALL roads that match some criteria?
# all_roads_fourlane = bcdc_query_geodata('digital-road-atlas-dra-master-partially-attributed-roads') %>%
#   filter(NUMBER_OF_LANES >= 4) %>%
#   filter(ROAD_SURFACE %in% c("paved","rough")) %>%
#   filter(ROAD_CLASS %in% c("local","highway","arterial","yield","collector","freeway")) %>%
#   collect() %>%
#   st_transform(crs = 4326)
#
# write_sf(all_roads_fourlane, 'RoadMortalityWebapp/www/roads_4lanes.gpkg')
#
# all_roads_two_or_more_lane = bcdc_query_geodata('digital-road-atlas-dra-master-partially-attributed-roads') %>%
#   filter(NUMBER_OF_LANES == 2) %>%
#   filter(ROAD_CLASS %in% c("local")) %>%
#   collect() %>%
#   st_transform(crs = 4326)

# iv. Hydrology layer (creek, stream, river, wetlands (I think the new wetland project is developing this layer…))
}
