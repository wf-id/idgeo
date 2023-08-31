#' Calculate Centroid Distance
#'
#' Draws a polygon around a set of points and calculates the distance and
#' summary statistics for each point to that centroid.
#'
#' This function takes a set of points and then draws the convex hull using
#' the \code{chull} function from grDevices. This hull is then converted to a polygon.
#' A centroid location is then determined from this polygon and the distance
#' from the centroid to each point is calculate.
#'
#' @param x a data.frame with columns for \code{x} and \code{y}
#' @param add_plot a boolean, draw a plot with the points and hull.
#'
#'
#' @return a list with items for:
#'   \describe{
#'   \item{distances_raw}{a matrix with the distance from the centroid for
#'       each points}
#'   \item{distances_sumz}{a data.frame with summary statistics}
#'   \item{cluster_centroid}{the centroid for the convex hull}
#'   \item{cluster_poly}{the polygon of the convex hull}
#'   }
#'
#' @export
#'
generate_average_distance_centroid <- function(x, add_plot = F){

  stopifnot(sum(c("x", "y") %in% names(x))==2)

  dat <- x[,c("x","y")]

  ch <- dat |>
    as.matrix() |>
    grDevices::chull()

  coords <- dat[c(ch, ch[1]), ]

  if(add_plot){
    graphics::plot(dat, pch=19)
    graphics::lines(coords, col="red")
  }

  sf_poly <- sf::st_polygon(list(as.matrix(coords)))

  cluster_centroid <- sf::st_sfc(sf_poly)

  cluster_centroid_poly <- cluster_centroid

  cluster_centroid <- sf::st_centroid(cluster_centroid)

  sf::st_crs(cluster_centroid) <- 4326

  distances <- sf::st_distance(sf::st_as_sf(dat, coords = c("x", "y"), crs = 4326) ,
                               cluster_centroid)
  requireNamespace("matrixStats", quietly = TRUE)
  distances_sumz <- data.frame(dist_avg = colMeans(distances, na.rm = TRUE),
                               dist_min = matrixStats::colMins(distances, na.rm = TRUE),
                               dist_max = matrixStats::colMaxs(distances, na.rm = TRUE),
                               dist_q05 = matrixStats::colQuantiles(distances, na.rm = TRUE, probs = .05),
                               dist_q95 = matrixStats::colQuantiles(distances, na.rm = TRUE, probs = .95),
                               dist_MAD = matrixStats::colMads(distances, na.rm = TRUE))

  #ud_units not exported from units anymore
  #distances_sumz <- purrr::map_df(distances_sumz, function(x) x * with(units::ud_units, m))

  list(distances_raw = distances,
       distances_sumz = distances_sumz,
       cluster_centroid = cluster_centroid,
       cluster_poly = cluster_centroid_poly)


}


