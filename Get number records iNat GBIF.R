library(rinat)
library(tidyverse)
library(sf)


# Download occurrences from the Road Mortality project on iNat.
# amph_and_reptiles_road_dat = rinat::get_inat_obs_project('94083', type = 'observations')
amph_and_reptiles_road_dat = rinat::get_inat_obs(query = 'quality_grade=research&identifications=any&place_id=7085&projects%5B%5D=canadian-amphibians-reptiles-on-roads')

# Localize them to BC.
bc_bound = bcmaps::bc_bound()

amph_sf = amph_and_reptiles_road_dat %>%
  filter(!is.na(latitude)) %>%
  st_as_sf(coords = c("longitude","latitude"), crs = 4326)

amph_bc = amph_sf %>%
  st_join(bc_bound %>% mutate(is_bc = TRUE) %>% st_transform(crs = 4326)) %>%
  filter(!is.na(is_bc))

# Which are our species for the loop?
dat = read_sf('RoadMortalityWebapp/www/herp_mortality_data.gpkg')
species = unique(dat$SCIENTIFIC)

results = tibble(species = species)

# for(species in species){
data_table = species %>%
  map( ~ {
  print(paste0('Working on species: ', .x))

  inat_observations = get_inat_obs(.x, maxresults = 10000) %>% as_tibble()
  print('Got the iNat results')

  number_inat_records = nrow(inat_observations)

  licenses_inat = inat_observations %>%
    count(license, sort = T) %>%
    mutate(license = replace(license, license == '', 'Unspecified')) %>%
    pivot_wider(names_from = license, values_from = n, names_prefix = 'inat_')

  # report quality
  report_quality_inat = inat_observations %>%
    count(quality_grade, sort = T) %>%
    pivot_wider(names_from = quality_grade, values_from = n, names_prefix = 'inat_quality_')

  # How about number of GBIF rows?
  number_gbif_records = rgbif::occ_count(scientificName = .x)

  # And how about that Road Mortality project on iNat?
  # number_road_mortality_proj_records = nrow(amph_and_reptiles_road_dat %>%
  #   filter(taxon.name == .x))

  number_road_mortality_proj_just_bc = nrow(amph_bc %>%
                                              filter(taxon.name == .x))

  # And licensing for this road mortality project points.
  licenses_rdmort = amph_bc %>%
    filter(taxon.name == .x) %>%
    st_drop_geometry() %>%
    count(license, sort = T) %>%
    mutate(license = replace(license, license == '' | is.na(license), 'Unspecified')) %>%
    pivot_wider(names_from = license, values_from = n, names_prefix = 'rdmort_bc_')

  out = tibble(species = .x,
               number_inat_records,
               number_gbif_records,
               number_road_mortality_proj_just_bc
               ) %>%
    bind_cols(licenses_inat, report_quality_inat,
              licenses_rdmort)
}) %>%
  bind_rows()

data_table_formatted = data_table %>%
  mutate(number_inat_records = as.character(number_inat_records)) %>%
  mutate(number_inat_records = replace(number_inat_records, number_inat_records == '10000', '10000+')) %>%
  mutate(across(-species, \(x) replace_na(x, 0)))

openxlsx::write.xlsx(data_table_formatted, 'tmp/number_of_records_and_license_types_iNat_GBIF.xlsx')
