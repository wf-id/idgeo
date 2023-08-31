
#' Local ZIP Codes
#'
#' Dataset of ZIP Codes in Guilford, Alamance, Rockingham, Randolph, Forsyth,
#'      Chatham, Stokes, Davidson, and Caswell Counties.
#'
#' @usage data(local_zips)
#' @format ## `local_zips`
#' A data frame with 154 rows and 8 columns:
#' \describe{
#' \item{ZIP}{ZIP Code}
#' \item{Classification}{ZIP Classification: General, P.O. Box, or Unique}
#' \item{City}{City}
#' \item{County}{County}
#' \item{Timezone}{Timezone}
#' \item{AreaCode}{String of Area Code(s), separated by \code{/}}
#' \item{Grouping}{ZIP Code Grouping for a higher grain than ZIP, but lower
#'      grain that county, for geographic analysis}
#' \item{CentralTo}{City that the ZIP is central to, if applicable}
#'
#' }
#'
#'
"local_zips"

#' North Carolina Counties
#'
#' Dataset of NC Counties & FIPS
#'
#' @usage data(nc_counties)
#' @format ## `nc_counties`
#' A data frame with 100 rows and 3 columns:
#' \describe{
#' \item{county}{County Name}
#' \item{county_code}{County FIPS Code (3-digits, no State ID)}
#' \item{GEOID}{State-County FIPS Code}
#' }

"nc_counties"


#' North Carolina Counties with Geometry
#'
#' Dataset of NC Counties with Geometry generated from
#'      \code{tigris::counties('NC')}. See \code{?tigris::counties} for info on
#'      all columns generated. Only partially documented below
#'
#' @usage data(sf_nc_counties)
#' @format ## `sf_nc_counties`
#' A sf data frame with 100 rows and 18 columns:
#' \describe{
#' \item{NAME}{County Name}
#' \item{COUNTYFP}{County FIPS Code (3-digits, no State ID)}
#' \item{GEOID}{State-County FIPS Code}
#' \item{geometry}{County geometry}
#' }

"sf_nc_counties"


#' North Carolina Tracts with Geometry
#'
#' Dataset of NC Tracts with Geometry generated from
#'      \code{tigris::tracts('NC')}. See \code{?tigris::tracts} for
#'      info on all columns generated. Only partially documented below
#'
#' @usage data(sf_nc_tracts)
#' @format ## `sf_nc_tracts`
#' A sf data frame with 2,672 rows and 13 columns:
#' \describe{
#' \item{GEOID}{Tract FIPS Code}
#' \item{geometry}{Tract geometry}
#' }

"sf_nc_tracts"

#' North Carolina ZCTAS with Geometry
#'
#' Dataset of NC ZCTAS with Geometry generated from
#'      \code{tigris::zctas('NC')}. See \code{?tigris::zctas} for
#'      info on all columns generated. Only partially documented below
#'
#' @usage data(sf_nc_zips)
#' @format ## `sf_nc_zips`
#' A sf data frame with 808 rows and 13 columns:
#' \describe{
#' \item{ZIP}{ZIP Code}
#' \item{geometry}{ZCTA geometry}
#' }

"sf_nc_zips"
