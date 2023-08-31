#' Put x/y points into polygons
#' @description identifies which polygon the given x/y coordinates fall in.
#'     The function will convert the data frame with the coordinates into an sf
#'     object so that a \code{st_within} join can be performed. Returned object will
#'     have the same number of rows as \code{df_pts}, all original columns from
#'     \code{df_pts}, along with any columns from \code{polygon}
#'
#' @param df_pts a data frame with columns 'x', 'y', or roughly 'lat', 'lon'
#' @param polygon an sf object that contains polygons in which to assign the
#'      x/y values to
#' @param keep_geometry default \code{FALSE} does not return geometry from the
#'      polygon data.
#'
#' @export
#'
put_pts_in_polygon <- function(df_pts, polygon,
                               keep_geometry = FALSE){
  requireNamespace('sf')

  if(!(all(c('x','y') %in% tolower(names(df_pts))) |
       all(c('lat','lon') %in% substr(tolower(names(df_pts)),1,3)) |
       all(c('lat','lng') %in% substr(tolower(names(df_pts)),1,3))
  )
  ){
    stop('No lat/lon or x/y columns detected in df_pts')
  } else if(all(c('x','y') %in% tolower(names(df_pts)))){
    cols <- sort(names(df_pts)[tolower(names(df_pts)) %in% c('x','y')])
    if(length(cols)>2)stop('more than 2 x/y columns detected:', paste(cols, collapse = ','))
    message('Using columns ',cols[1],' and ',cols[2],' for longitude & latitude')

  } else if(all(c('lat','lon') %in% substr(tolower(names(df_pts)),1,3))){
    cols <- rev(sort(names(df_pts)[substr(tolower(names(df_pts)),1,3) %in% c('lat','lon')]))
    if(length(cols)>2)stop('more than 2 lat/lon columns detected')
    message('Using columns ',cols[1],' and ',cols[2],' for longitude & latitude')

  } else if(all(c('lat','lng') %in% substr(tolower(names(df_pts)),1,3))){
    cols <- rev(sort(names(df_pts)[substr(tolower(names(df_pts)),1,3) %in% c('lat','lng')]))
    if(length(cols)>2)stop('more than 2 lat/lng columns detected')
    message('Using columns ',cols[1],' and ',cols[2],' for longitude & latitude')
  } else {
    stop('Invalid lat/lon or x/y columns detected in df_pts')
  }

  pts_sf <- df_pts |>
    sf::st_as_sf(coords = cols, crs = 4326,
                 remove = FALSE)

  pts_sf <- sf::st_transform(pts_sf, sf::st_crs(polygon))

  pts_out <- sf::st_join(pts_sf,
                     polygon, join = sf::st_within,
                     left = T)

  if(keep_geometry){
    return(pts_out)
  }else {
    return(pts_out |> sf::st_drop_geometry())
    }

}
