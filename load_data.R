library(tidyverse)
library(sf)
library(bcdata)
library(bcmaps)

# Set up directories to store data: /data, /www

if(!dir.exists('data')) dir.create('data')
if(!dir.exists('RoadMortalityWebapp/www')) dir.create('RoadMortalityWebapp/www')

#############
# Load data #
#############

# Spatial layers from {bcdata}
# i. BC
bc = bcmaps::bc_bound()

# ii. Different regions/districts/boundaries within Province
bcmaps::nr_regions() %>%
  st_simplify(dTolerance = 200) %>%
  rename(shape_name = REGION_NAME) %>%
  dplyr::select(shape_name) %>%
  write_sf('RoadMortalityWebapp/www/nr_regions.gpkg')

bcmaps::nr_districts() %>%
  st_simplify(dTolerance = 200) %>%
  rename(shape_name = DISTRICT_NAME) %>%
  dplyr::select(shape_name) %>%
  write_sf('RoadMortalityWebapp/www/nr_districts.gpkg')

bcmaps::ecoprovinces() %>%
  st_simplify(dTolerance = 200) %>%
  rename(shape_name = ECOPROVINCE_NAME) %>%
  mutate(shape_name = stringr::str_to_title(shape_name)) %>%
  dplyr::select(shape_name) %>%
  write_sf('RoadMortalityWebapp/www/ecoprovinces.gpkg')

bcmaps::ecoregions() %>%
  st_simplify(dTolerance = 200) %>%
  rename(shape_name = ECOREGION_NAME) %>%
  mutate(shape_name = stringr::str_to_title(shape_name)) %>%
  dplyr::select(shape_name) %>%
  write_sf('RoadMortalityWebapp/www/ecoregions.gpkg')

bcmaps::ecosections() %>%
  st_simplify(dTolerance = 200) %>%
  rename(shape_name = ECOSECTION_NAME) %>%
  dplyr::select(shape_name) %>%
  write_sf('RoadMortalityWebapp/www/ecosections.gpkg')

# # Species of interest
# species_of_int = openxlsx::read.xlsx('Species of Interest.xlsx')
# sci_names = species_of_int$Scientific.Name

# Mortality Events data
# i. KMZ / KML / GDB files from Leigh Anne.
occ_data_zipped = list.files('data/OccurrenceData/',full.names = T)

kmz_layers = occ_data_zipped[str_detect(occ_data_zipped,'\\.kmz')] %>%
  as.list() %>%
  map( ~ read_sf(unzip(.x)))
# list elements 1 and 2 are identical. 3rd element empty. 4th is usable. 5th has no written location.
# No need to use the kmz layers.

kml_layers = occ_data_zipped[str_detect(occ_data_zipped,'\\.kml')] %>%
  as.list() %>%
  map( ~ read_sf(.x)) %>%
  bind_rows()
# KML layer is just fourth element of KMZ, perfect.

kml_layers = kml_layers %>%
  mutate(Description = str_replace(Description, 'Locatoin','Location'))

herp_mortality_gdb = read_sf('data/OccurrenceData/SpatialFiles_AmphibRepti_mortalities.gdb/')
# Looks great, lots of info there.

write_sf(kml_layers, 'RoadMortalityWebapp/www/KML_roadmort.gpkg')
write_sf(herp_mortality_gdb, 'data/herp_mortality_data_from_leighanne.gpkg')

morts = read_sf('data/herp_mortality_data_from_leighanne.gpkg')


# 2. iNaturalist; specifically, this project: https://inaturalist.ca/projects/canadian-amphibians-reptiles-on-roads
inat_data = list.files(path = 'data/OccurrenceData/',
           pattern = 'canadian-amphibians-.*.csv',
           full.names = T) |>
  lapply(read_csv) |>
  bind_rows()

sci_names = unique(inat_data$scientific_name)

inat_data = inat_data |>
  filter(scientific_name %in% sci_names)


# Adjust format to match 'herp_mortality_data.gpkg'

# 3. FrogWatch

# Waiting on Megan for this one.

# 4. SPI
spi_test = bcdc_query_geodata('wildlife-species-inventory-incidental-observations-non-secured') |>
  filter(SCIENTIFIC_NAME %in% sci_names) |>
  collect()

spi_mort_data = spi_test |>
  filter(str_detect(OBSERVATION_COMMENTS, "([d,D]ead|[c,C]arcass|[m,M]ort(ality)?)"))

spi_mort_data |>
  dplyr::select(starts_with("PROJECT")) |>
  st_drop_geometry() |>
  write_csv('output/SPI_project_numbers.csv')
# I've sent the above CSV file to Megan.

# 4. Regional Bios etc.

# Spatial files from BCG Warehouse and simplify the geometries before we visualize.
bcdata_list = bcdc_list()

# i. BC Parks/protected areas
if(!file.exists('data/parks_fullres.gpkg')){
  parks = bcdc_query_geodata('terrestrial-protected-areas-representation-by-ecosection-parc-') %>%
  collect()
  write_sf(parks,'data/parks_fullres.gpkg')
}else{
  parks = read_sf('data/parks_fullres.gpkg')
}

parks_simple = st_simplify(parks, dTolerance = 200)
ggplot() + geom_sf(data = parks_simple)
file.remove('RoadMortalityWebapp/www/parks_simplified.gpkg')
write_sf(parks_simple, 'RoadMortalityWebapp/www/parks_simplified.gpkg')
# ggplot() + geom_sf(data = parks)



# Must make sure datasets can be combined.

morts_for_join = morts |>
  reframe(incidental = INCIDENTAL,
         species_code = SPECIES_CO,
         common_name = SPECIES_EN,
         scientific_name = SCIENTIFIC,
         BC_see_spec = BCSEE_SPEC,
         date_time = OBSERVATIO,
         year = OBSERVAT_1,
         month = OBSERVAT_2,
         day = OBSERVAT_3,
         data_source = 'Leigh-Anne',
         proj_name = PROJECT_NA,
         proj_id = PROJECT_ID,
         proj_web = PROJECT_WE,
         utm_zone = UTM_ZONE,
         easting = UTM_EASTIN,
         northing = UTM_NORTHI,
         lat = LATITUDE,
         lng = LONGITUDE,
         adult_male = ADULT_MALE,
         adult_female = ADULT_FEMA,
         juv_male = JUVENILE_M,
         juv_female = JUVENILE_F,
         sign_or_activity = SIGN_OR_AC,
         comments = OBSERVAT_5,
         status = PROVINCI_1,
         cosewic = COSEWIC_CD,
         class_name = CLASS_NAME,
         geometry = geom
         )
morts_for_join = st_set_geometry(morts_for_join, morts_for_join$geometry)
morts_for_join = st_transform(morts_for_join,crs = 4326)


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

all_mort_data = morts_for_join |>
  bind_rows(spi_mort_data_for_join) |>
  bind_rows(inat_data_for_join) |>
  distinct()

write_sf(all_mort_data,
         'RoadMortalityWebapp/www/herp_mortality_data.gpkg')
# I've commented out the section below... I haven't removed the data
# from the app's /www folder yet, but probably will, as the code below
# only accesses and simplifies geometries for a piddly number of road mortality events;
# if we try to do the following for all the iNaturalist / SPI data, I think it
# might be too big a hassle / the dataset(s) might be too large!

# # ii. Culverts. Note: Leigh Anne: "I also learned from Karina where the 'best'
# #     culvert spatial file is. It is: BCGW in something called 'criss mapping'.
# #     I may have this word TOTALLY misspelled. There is a file that refers to culvert
# #     mapping."
# bcdata_list[str_detect(bcdata_list,'culvert')]
#
#
# # iii. Roads (highways and gravel roads; FSR if available)
# bcdata_list[str_detect(bcdata_list,'road-atlas')]
#
# # Query the giant roads spatial file by row, to avoid downloading a huge chunk of roads.
# # Here, we just cycle through each road mortality incident, buffering 100m around the
# # occurrence point of the mortality incident, and downloading any roads within that radius.
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
#
# #Write to disk.
# write_sf(all_mort_roads_within_100m,'RoadMortalityWebapp/www/roads_within_100m.gpkg')
#
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
