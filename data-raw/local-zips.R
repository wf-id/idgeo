## code to prepare `local_zips` dataset
local_zips <- readxl::read_xlsx(here::here('data-raw', 'LocalZipCodes.xlsx'))
local_zips <- local_zips[,c(-1,-6)] #remove link, Population columns
names(local_zips) <- c("ZIP", "Classification", "City", "County", "Timezone",
                       "AreaCodes", "Grouping", "CentralTo")
usethis::use_data(local_zips, overwrite = T)
