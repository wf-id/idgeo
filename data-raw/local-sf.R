library(sf)
options(tigris_use_cache=TRUE)

nc_counties <- tigris::list_counties('NC')
nc_counties$GEOID <- paste0('37', nc_counties$county_code)

usethis::use_data(nc_counties, overwrite = T)

sf_nc_counties <- tigris::counties(state = 'NC')

usethis::use_data(sf_nc_counties, overwrite = T)

sf_nc_tracts <- tigris::tracts(state = 'NC')

usethis::use_data(sf_nc_tracts, overwrite = T)

sf_nc_zips <- tigris::zctas(state = 'NC', year = 2010)
sf_nc_zips$ZIP <- sf_nc_zips$ZCTA5CE10

usethis::use_data(sf_nc_zips, overwrite = T)
