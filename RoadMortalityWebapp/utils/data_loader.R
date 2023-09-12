# Load data.
rmdat = read_sf('www/herp_mortality_data.gpkg') |>
  mutate(common_name = case_when(
    common_name %in% c("Common Gartersnake","Garter Snakes") ~ "Garter Snake",
    common_name == "Desert Night Snake deserticola" ~ "Desert Nightsnake",
    T ~ common_name
  )) |>
  mutate(common_name = stringr::str_to_title(common_name)) |>
  # mutate(proj_name = ifelse(proj_name == 'Incidental Observations', 'SPI - Incidental Observation', proj_name)) |>
  st_transform(4326)

kml_rmdat = read_sf('www/KML_roadmort.gpkg')

nr_regions = read_sf('www/nr_regions.gpkg')

nr_districts = read_sf('www/nr_districts.gpkg')

ecoprovs = read_sf('www/ecoprovinces.gpkg')

ecosects = read_sf('www/ecosections.gpkg')

ecoregions = read_sf('www/ecoregions.gpkg')

parks = read_sf('www/parks_simplified.gpkg')

roads_in_100m = read_sf('www/roads_within_100m.gpkg')
