% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/leaflet_controls.R
\name{addIDProviderTiles}
\alias{addIDProviderTiles}
\title{Leaflet Provider Tile Default for ID}
\usage{
addIDProviderTiles(
  map,
  minimal = TRUE,
  options = IDTileOptions(),
  provider = NULL,
  ...
)
}
\arguments{
\item{map}{the map object}

\item{minimal}{whether to choose the minimal (Stadia.AlidadeSmooth, default) or more
detailed default map tiles Stadia.OSMBright. This is ignored if \code{provider}
is explicitly provided}

\item{options}{defaults to \code{\link[idgeo]{IDTileOptions}}}

\item{provider}{default \code{NULL} will use provider based on \code{minimal} argument}

\item{...}{any other arguments to \code{\link[leaflet]{addProviderTiles}}}
}
\description{
add the default provider tile for ID Research
}
