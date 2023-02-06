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
  dplyr::select(shape_name) %>%
  write_sf('RoadMortalityWebapp/www/ecoprovinces.gpkg')

bcmaps::ecoregions() %>%
  st_simplify(dTolerance = 200) %>%
  rename(shape_name = ECOREGION_NAME) %>%
  dplyr::select(shape_name) %>%
  write_sf('RoadMortalityWebapp/www/ecoregions.gpkg')

bcmaps::ecosections() %>%
  st_simplify(dTolerance = 200) %>%
  rename(shape_name = ECOSECTION_NAME) %>%
  dplyr::select(shape_name) %>%
  write_sf('RoadMortalityWebapp/www/ecosections.gpkg')


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
write_sf(herp_mortality_gdb, 'RoadMortalityWebapp/www/herp_mortality_data.gpkg')

morts = read_sf('RoadMortalityWebapp/www/herp_mortality_data.gpkg')

# 2. iNaturalist

# 3. FrogWatch

# 4. SPI

# 5. Regional Bios etc.

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

# ii. Culverts. Note: Leigh Anne: "I also learned from Karina where the 'best'
#     culvert spatial file is. It is: BCGW in something called 'criss mapping'.
#     I may have this word TOTALLY misspelled. There is a file that refers to culvert
#     mapping."
bcdata_list[str_detect(bcdata_list,'culvert')]


# iii. Roads (highways and gravel roads; FSR if available)
bcdata_list[str_detect(bcdata_list,'road-atlas')]

# Query the giant roads spatial file by row, to avoid downloading a huge chunk of roads.
# Here, we just cycle through each road mortality incident, buffering 100m around the
# occurrence point of the mortality incident, and downloading any roads within that radius.
mortality_road_within_100m = herp_mortality_gdb$INCIDENTAL %>%
  as.list() %>%
  map(., ~ {
    tryCatch(
    expr = bcdc_query_geodata('digital-road-atlas-dra-master-partially-attributed-roads') %>%
      filter(INTERSECTS(st_buffer(herp_mortality_gdb[herp_mortality_gdb$INCIDENTAL == .x,],dist = 100))) %>%
      collect() %>%
      mutate(related_mortality = .x) %>%
      dplyr::select(related_mortality, DIGITAL_ROAD_ATLAS_LINE_ID, FEATURE_TYPE, SEGMENT_LENGTH_2D, ROAD_SURFACE, ROAD_CLASS, DATA_CAPTURE_DATE) %>%
      setNames(snakecase::to_snake_case(names(.))) %>%
      mutate(across(-geometry, as.character)),
    error = function(e) {
      #print(paste0("error - no data available for herp mortality incidental number: ", .x))
      # data.frame(related_mortality = .x, lat = NA, lon = NA) %>%
      #   st_as_sf(coords = c('lon','lat'), crs = 4326) %>%
      #   st_transform(crs = 3005)
    })
  })

all_mort_roads_within_100m = bind_rows(mortality_road_within_100m)

#Write to disk.
write_sf(all_mort_roads_within_100m,'RoadMortalityWebapp/www/roads_within_100m.gpkg')

roads = read_sf('RoadMortalityWebapp/www/roads_within_100m.gpkg')

ggplot() + geom_sf(data = roads %>%
  filter(related_mortality == '58234'),
  aes(col = road_class)) +
  geom_sf(data = morts %>% filter(INCIDENTAL == '58234'))

ggplot() + geom_sf(data = roads)
# iv. Hydrology layer (creek, stream, river, wetlands (I think the new wetland project is developing this layer…))
