#' Leaflet Tile Options for ID
#' @description default tile options ID Research
#' @param minZoom min zoom value, default \code{6}
#' @param maxZoom max zoom value, default \code{16}
#' @param ... any other arguments to \code{\link[leaflet]{tileOptions}}
#'
#' @export
#'
#'
IDTileOptions <- function(minZoom = 6, maxZoom = 16, ...){
  leaflet::tileOptions(minZoom = minZoom, maxZoom = maxZoom, ...)
}

#' Leaflet Provider Tile Default for ID
#' @description add the default provider tile for ID Research
#' @param map the map object
#' @param minimal whether to choose the minimal (Stadia.AlidadeSmooth, default) or more
#'      detailed default map tiles Stadia.OSMBright. This is ignored if \code{provider}
#'      is explicitly provided
#' @param options defaults to \code{\link[idgeo]{IDTileOptions}}
#' @param provider default \code{NULL} will use provider based on `minimal` argument
#' @param ... any other arguments to \code{\link[leaflet]{addProviderTiles}}
#'
#' @export
#'
#'
addIDProviderTiles <- function(map,
                               minimal = TRUE,
                               options = IDTileOptions(),
                               provider = NULL,
                               ...){

  if(missing(provider)) provider <- ifelse(minimal, 'Stadia.AlidadeSmooth',
                                           'Stadia.OSMBright')

  leaflet::addProviderTiles(map = map, provider = provider,
                            options = options, ...)
}


#' Leaflet Label Options Defaults for ID
#' @description Some functions that will override some of the 'leaflet' defaults
#'    See the full documentation for \code{\link[leaflet]{labelOptions}} for details
#' @param textsize new default is \code{'14px'}
#' @param opacity new default is \code{0.85}
#' @param fontcolor font color for label, default from \code{idstyle} package
#' @param fontfamily font family for label, default from \code{idstyle} package
#' @param additionalstyles additional named list of styles (not color or font-family) passed to \code{style} argument of \code{\link[leaflet]{labelOptions}}
#' @param ... additional arguments passed to \code{\link[leaflet]{labelOptions}}
#'
#' @export
#'
#'

IDLabelOptions <- function(textsize = '14px',
                           opacity = 0.85,
                           fontcolor = idstyle::wake_darkgold,
                           fontfamily = idstyle::wake_sansserif,
                           additionalstyles = NULL,
                           ...){

  style <- leaflet::filterNULL(list("color" = fontcolor,
                                    "font-family" = fontfamily,
                                    additionalstyles))

  leaflet::labelOptions(textsize = textsize, opacity = opacity,
                        style = style, ...)
}



#' Hospital Icons
#' @description This function creates an automatic icon to be used with leaflet maps
#'     that represents hospitals
#'     Submit \code{leaflet::makeAwesomeIcon} to see the full
#'     documentation for \code{\link[leaflet]{makeAwesomeIcon}}
#' @param iconColor default of white
#' @param ... any additional `makeAwesomeIcon` arguments
#' @export

hospitalIcon <- function(iconColor = "#FFFFFF", ...){
  leaflet::makeAwesomeIcon(icon = 'h-square',library = 'fa',
                           iconColor = iconColor, ...)
}

#' Ambulance Icons
#' @description This function creates an automatic icon to be used with leaflet maps
#' that represents an ambulance
#' Submit \code{leaflet::makeAwesomeIcon} to see the full
#'     documentation for \code{\link[leaflet]{makeAwesomeIcon}}
#' @param iconColor default of white
#' @param ... any additional `makeAwesomeIcon` arguments
#' @export

ambulanceIcon <- function(iconColor = "#FFFFFF", ...){
  leaflet::makeAwesomeIcon(icon = 'ambulance',library = 'fa',
                           iconColor = iconColor, ...)
}

#' Multiple Base Legend Support for Leaflet Maps
#' @description This function allows for multiple base layer legends that toggle
#'      with selection. Default \code{leaflet} shows all legends regardless of
#'      which base layer is selected.
#'      Note that the *title* of the legend for each layer
#'      (\code{addLegend(title = 'Some Layer')}) must be the same as the *group*
#'      name of the layer (\code{addLayersControl(baseGroups = c('Some Layer'))}).
#'      If there are any *overlay layers*, this function also ensures they
#'      remain in front of the multiple *base layers* when toggling.
#'
#' @param map a \code{leaflet} map
#' @param mapnum optional, identifies the map number when multiple maps are
#'      rendered in the same document. Used in conjunction with \code{map_id},
#'      this fixes any javascript rendering issues that might otherwise
#'      incorrectly impact multiple maps. The actual numbers are irrelevant as
#'      long as each map has a unique number. Defaults to \code{1}
#' @param map_id optional, defaults to \code{NULL}. A unique string identifier
#'      of the map (any spaces will be removed) when multiple maps are rendered
#'      in the same document. Used in conjunction with \code{mapnum} to fix
#'      javascript rendering issues
#'
#' @export

multiBaseLegends <- function(map, mapnum = 1, map_id=NULL){

  if(!is.null(map_id)) map_id <- gsub('\\s','',map_id)
  requireNamespace("htmlwidgets", quietly = TRUE)
  map |>
    htmlwidgets::onRender(paste0("
    function(el, x) {",
    ifelse(is.null(map_id),
           "let elem = document;",
           paste0("let elem = document.getElementById('",map_id,"');")),
    "
      var updateLegend",mapnum," = function () {
          var selectedGroup = elem.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);

          elem.querySelectorAll('.legend').forEach(a => a.hidden=true);
          elem.querySelectorAll('.legend').forEach(l => {
            if (l.children[0].children[0].innerText == selectedGroup) l.hidden=false;
          });
      };
      updateLegend",mapnum,"();
      this.on('baselayerchange', e => updateLegend",mapnum,"());
    }"))
}

#' Fix NA in Leaflet Legend
#' @description  This function corrects the odd alignment of \code{NA} that
#'      appears in some \code{leaflet} legends. Instead of floating in the right
#'      side of the legend, it will be correctly aligned at the bottom of the
#'      legend.
#'
#' @param map a \code{leaflet} map
#'
#' @export

legendFixNA <- function(map){
  requireNamespace("htmlwidgets", quietly = TRUE)
  requireNamespace("htmltools", quietly = TRUE)
  css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
  html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML
  map |> htmlwidgets::prependContent(html_fix)
}


#' Align Leaflet Controls
#'
#' @description  This function allows for side by side \code{leaflet} controls
#' instead of the default horizontal arrangement. Especially useful with
#' \code{\link[idgeo]{addTitleControl}} so the "topleft" position isn't below
#' the zoom controls.
#'
#' @param map a \code{leaflet} map
#' @param direction defaults to \code{horizontal}, and is intended to be used
#'      without arguments since a choice of \code{vertical}
#'      (\code{leaflet} default) simply returns the map as is.
#'
#' @export

alignControls <- function(map, direction = c('horizontal', 'vertical')){
  d <- match.arg(direction)
  if(d=='vertical') return(map)
  requireNamespace("htmltools", quietly = TRUE)
  requireNamespace("htmlwidgets", quietly = TRUE)
  #float: none;
  #background-color: rgba(255,255,255,0);
  map |>
    htmlwidgets::prependContent(htmltools::HTML('<style type = "text/css">
                                                .leaflet-control {clear:inherit;}
                                                .leaflet-control-attribution.leaflet-control {
                                                  clear:both;
                                                  }
                                                </style>'))
}


#' Create HTML Title
#'
#' @description  This function creates a formatted HTML block intended for use
#'      as a title block in an HTML widget. For adding a title directly to a
#'      leaflet map, see \code{\link[idgeo]{addTitleControl}}
#'
#' @param title a character string used as the title. Defaults to \code{NULL}.
#'      Either \code{title} or \code{subtitle} is required.
#' @param subtitle a character string used as the subtitle. Defaults to
#'      \code{NULL}. Either \code{title} or \code{subtitle} is required.
#' @param titlesize number indicating size of the title in pixels. Default is
#'      \code{22}.
#' @param subtitlesize number indicating size of the subtitle in pixels.
#' Default is \code{16}.
#' @param titlebold logical indicating whether or not the title should be bold.
#'      defaults to \code{TRUE}
#' @param subtitlebold logical indicating whether or not the title should be
#'      bold. defaults to \code{TRUE}
#' @param titlecolor character string indicating a valid color for the title.
#'      Defaults to black.
#' @param subtitlecolor character string indicating a valid color for the
#'      subtitle. Defaults to primary from \code{idstyle} package.
#' @param titlefont character string indicating a valid font family for the
#'      title. Defaults to sans serif from \code{idstyle} package.
#' @param subtitlefont character string indicating a valid font family for the
#'      subtitle. Defaults to sans serif from \code{idstyle} package.
#' @param padding number supplying amount of padding in pixels around the title
#'      block. Default is \code{10}.
#'
#' @export

htmlTitle <- function(title = NULL, subtitle = NULL,
                      titlesize = 22, subtitlesize = 16,
                      titlebold = T, subtitlebold = T,
                      titlecolor = "black",
                      subtitlecolor = idstyle::wake_gold,
                      titlefont = idstyle::wake_sansserif,
                      subtitlefont = idstyle::wake_sansserif,
                      padding = 10){

  if(is.null(title) & is.null(subtitle)) stop('title or subtitle must be supplied')
  if(!is.numeric(titlesize)|titlesize<=0) stop('titlesize must be a positive integer.')
  if(!is.numeric(subtitlesize)|subtitlesize<=0) stop('subtitlesize must be a positive integer.')
  if(!is.numeric(padding)|padding<=0) stop('padding must be a positive integer.')
  if(!is.logical(titlebold)) stop('titlebold must be logical.')
  if(!is.logical(subtitlebold)) stop('subtitlebold must be logical.')

  titlestyle <- paste0("color:",titlecolor,"; ",
                       "font-size:",titlesize,"px; ",
                       "font-family:",titlefont,';')
  subtitlestyle <- paste0("color:",subtitlecolor,"; ",
                          "font-size:",subtitlesize,"px; ",
                          "font-family:",subtitlefont,';')
  if(!is.null(title)) {
    titlespan <- paste0("<span style = '",titlestyle,"'>",
                        ifelse(titlebold,'<b>',''),
                        title,
                        ifelse(titlebold,'</b>',''),
                        "</span>")
  } else titlespan <- NULL
  if(!is.null(subtitle)) {
    subtitlespan <- paste0("<span style = '",subtitlestyle,"'>",
                           ifelse(subtitlebold,'<b>',''),
                           subtitle,
                           ifelse(subtitlebold,'</b>',''),
                           "</span>")
  }else subtitlespan <- NULL

  if(is.null(title)) {
    out <- paste0("<div style='padding:",padding,"px;'>",
                  subtitlespan,"</div>")
  } else if(is.null(title)) {
    out <- paste0("<div style='padding:",padding,"px;'>",
                  titlespan,"</div>")
  } else {
    out <- paste0("<div style='padding:",padding,"px;'>",
                  titlespan,'<br>', subtitlespan,"</div>")

  }

  requireNamespace("htmltools", quietly = TRUE)
  htmltools::HTML(out)

}

#' Add Title Control to Leaflet Map
#'
#' @description  This function adds a formatted HTML title to a \code{leaflet}
#'      map. For a standalone HTML title block, see \code{\link[idgeo]{htmlTitle}}
#'
#' @param map an existing \code{leaflet} map.
#' @param title a character string used as the title. Defaults to \code{NULL}.
#'      Either \code{title} or \code{subtitle} is required.
#' @param subtitle a character string used as the subtitle. Defaults to
#'      \code{NULL}. Either \code{title} or \code{subtitle} is required.
#' @param position position of control: "topleft", "topright", "bottomleft", or
#'      "bottomright"
#' @param layerId the leaflet layerid
#' @param ... additional HTML options (\code{titlesize}, \code{titlecolor},
#'      etc.) to pass to \code{\link[idgeo]{htmlTitle}}. See \code{?idgeo::htmlTitle}
#'      for full documentation
#'
#' @export

addTitleControl <- function(map, title = NULL, subtitle = NULL,
                            position = c("topleft", "topright", "bottomleft",
                                         "bottomright"),
                            layerId = NULL,
                            ...){

  out_html <- idgeo::htmlTitle(title = title, subtitle = subtitle, ...)

  map |> leaflet::addControl(html = out_html, position = position,
                             className = 'info', layerId = layerId)


}


#' Percentage Color Mapping
#'
#' @description Maps values that are known to be between 0 & 1 to a given
#'      palette using \code{\link[leaflet]{colorNumeric}}
#'
#' @param palette The colors or color function that values will be mapped to.
#'      Defaults to \code{'viridis'}
#' @param domain The possible values that can be mapped. Defaults to
#'      \code{c(0,1)}
#' @param ... other options passed to \code{\link[leaflet]{colorNumeric}}
#'
#' @export

colorPercent <- function(palette = 'viridis', domain = c(0,1), ...){
  leaflet::colorNumeric(palette = palette, domain = domain, ...)


}


#' Percentage Label Format for Leaflet Legend
#'
#' @description A function that automatically formats a leaflet legend as a
#'      percentage. A special case of \code{\link[leaflet]{labelFormat}} with
#'      \code{suffix = "%"} and transformation of \code{100*x}
#'
#' @param digits the number of digits in the percentage
#' @param ... other arguments passed to \code{\link[leaflet]{labelFormat}}
#'
#' @export

labelFormatPercent <- function(digits = 0, ...){
  leaflet::labelFormat(suffix = '%',
                       transform = function(x)round(100*x, digits = digits), ...)

}


#' Change Fonts used in Leaflet Map
#'
#' @description  This function changes the default font of a \code{leaflet} map.
#'
#' @param map an existing \code{leaflet} map.
#' @param font a character string specifying the font-family. Defaults to
#'      sans serif from \code{idstyle} package
#' @param tooltip_color character string indicating color of tooltip. default of
#'      primary dark from \code{idstyle} package
#'
#' @export
leafletFont <- function(map,
                        font = idstyle::wake_sansserif,
                        tooltip_color = idstyle::wake_darkgold){
  requireNamespace("htmltools", quietly = TRUE)
  requireNamespace("htmlwidgets", quietly = TRUE)
  map |> htmlwidgets::prependContent(htmltools::HTML(paste0('<style type = "text/css">',
                                                            '.leaflet-container .info, .leaflet-container {font-family: ',
                                                            font,
                                                            ';}',
                                                            '.leaflet-tooltip {',
                                                            'color: ',tooltip_color,';',
                                                            '}',
                                                            '</style>')))
}

#' Use Font Awesome
#'
#' @description  Applies header info for using fontawesome icons (newer version
#' than what comes with leaflet by default)
#'
#' @param map a leaflet map
#'
#' @export
useFontAwesome <- function(map){

  fa_script <- '<script src="https://kit.fontawesome.com/1ffe760482.js" crossorigin="anonymous"></script>'
  css_fix <- '<style type = "text/css"> .legend i {opacity: 1;} </style>'

  map |>
    htmlwidgets::prependContent(htmltools::HTML(css_fix)) |>
    htmlwidgets::prependContent(htmltools::HTML(fa_script))

}


#' Pretty Leaflet Maps
#'
#' @description  This function can be used in place of
#'      \code{\link[leaflet]{leaflet}} and will apply many helper and style
#'      functions for quick consistent maps. Each added option can be removed
#'      by setting the argument to \code{FALSE}
#'
#' @param tiles logical, default \code{TRUE}, whether or not to use
#'      \code{\link[idgeo]{addIDProviderTiles}}, the Wake ID default map layer.
#' @param tilesminimal whether to use the minimal or more detailed Wake ID default
#'      map tiles. Ignored if \code{tiles} is \code{FALSE}
#' @param align logical, default \code{TRUE}, whether or not to use
#'      \code{\link[idgeo]{alignControls}}, horizontal alignment of multiple
#'      \code{leaflet} controls.
#' @param legendNA logical, default \code{TRUE}, whether or not to use
#'      \code{\link[idgeo]{legendFixNA}}, which corrects odd placement of
#'      \code{NA} values in the legend.
#' @param font logical, default \code{TRUE}, whether or not to use
#'      \code{\link[idgeo]{leafletFont}}, the wake default font face through
#'      the map.
#' @param fullscreen logical, default \code{TRUE}, whether or not to use
#'      \code{\link[leaflet.extras]{addFullscreenControl}}, a fullscreen control
#'      button
#' @param multibase logical, default \code{FALSE}, whether or not to use
#'      \code{\link[idgeo]{multiBaseLegends}}, support for multiple base layer
#'      legends that toggle appropriately. If you are not careful with the
#'      naming of legends, this may make your legends disappear, and is thus not
#'      on by default.
#' @param idbtn default \code{TRUE} whether or not to add easy button with IDEAS logo & info to the map
#' @param btnposition position of IDEAS button. ignored
#'      if \code{idbtn} is set to \code{FALSE}
#' @param btnextrainfo extra html to include in the button popup on click. ignored
#'      if \code{idbtn} is set to \code{FALSE}
#' @param mini whether to add to mini map via \code{\link[leaflet]{addMiniMap}}
#' @param miniposition position of mini map. ignored
#'      if \code{mini} is set to \code{FALSE}
#' @param ... any parameters supplied to \code{\link[leaflet]{leaflet}}
#'
#' @export
prettyLeaflet <- function(...,
                          tiles = T,
                          tilesminimal = T,
                          align = T, legendNA = T,
                          font = T, fullscreen = T, multibase = F,
                          idbtn = T, btnposition = 'bottomleft',
                          btnextrainfo = NULL,
                          mini = F,
                          miniposition = 'topright'){

  requireNamespace("leaflet", quietly = TRUE)

  addPT <- function(map, yes){
    if(yes) idgeo::addIDProviderTiles(map,minimal = tilesminimal) else map
  }
  alignC <- function(map, yes){
    if(yes) idgeo::alignControls(map) else map
  }
  legendFNA<- function(map, yes){
    if(yes) idgeo::legendFixNA(map) else map
  }
  leafletF <- function(map, yes){
    if(yes) idgeo::leafletFont(map) else map
  }
  addFS <- function(map, yes){
    requireNamespace("leaflet.extras", quietly = TRUE)
    if(yes)leaflet.extras::addFullscreenControl(map) else map
  }
  multiBL <- function(map, yes){
    if(yes) idgeo::multiBaseLegends(map) else map
  }
  idBut <- function(map, yes){
    if(yes) idgeo::addIdeasButton(map, position = btnposition,
                                  extrainfo = btnextrainfo) else map
  }

  addMini <- function(map, yes){
    if(yes)leaflet::addMiniMap(map, position = miniposition) else map
  }

  leaflet::leaflet(...) |>
    addPT(yes = tiles) |>
    alignC(yes = align) |>
    legendFNA(yes = legendNA) |>
    leafletF(yes = font) |>
    addFS(yes = fullscreen) |>
    multiBL(yes = multibase) |>
    idgeo::useFontAwesome()|>
    idBut(yes = idbtn) |>
    addMini(yes = mini)

}

#' Quick Leaflet Palette
#'
#' @description  This function will generate a palette by either
#'      \code{\link[leaflet]{colorNumeric}} or \code{\link[leaflet]{colorFactor}}
#'      for numeric and character/factor vectors respectively. When \code{percent = TRUE}
#'      \code{\link[idgeo]{colorPercent}} will be used instead
#'
#' @param x a vector
#' @param palette optional, a string specifying a color palette function or a
#'      vector of colors; default is 'viridis' for numeric or percent
#'      and Okabe-Ito for character or factor (note: Okabe-Ito only supports up to 9
#'      distinct factor levels)
#' @param percent default \code{NULL}
#' @param reverse whether to reverse the \code{palette}
#' @param domain domain used for the \code{palette}. the default \code{domain = NULL} will
#'      use all observations in \code{x} as the range, except when \code{percent = TRUE} in
#'      which case the domain defaults to \code{c(0,1)}
#'
#' @export
quickPal <- function(x, palette = NULL, percent = FALSE,
                     reverse = FALSE, domain = NULL){

  stopifnot(is.vector(x))
  stopifnot(is.logical(percent))
  stopifnot(is.logical(reverse))

  zcolclass <- class(x)

  if(is.null(domain)) domain <- setdomain(x, percent = percent)

  pal <- if(percent){

    if(is.null(palette)) palette <- 'viridis'

    idgeo::colorPercent(palette = palette,
                        domain = domain,
                        reverse = reverse)

  } else if('numeric' %in% zcolclass){

    if(is.null(palette)) palette <- 'viridis'

    leaflet::colorNumeric(palette = palette,
                          domain = domain,
                          reverse = reverse)
  } else {

    zcollevels <- length(stats::na.omit(unique(x)))

    if(is.null(palette)) palette <- grDevices::palette.colors(n = zcollevels,
                                                              palette = "Okabe-Ito")

    leaflet::colorFactor(palette = palette,
                         domain = domain,
                         reverse = reverse
    )
  }

  return(pal)

}


setdomain <- function(x, percent = FALSE){

  if(percent){
    return( c(0,1) )
  } else {
    return(x)
  }

}

#' Pretty Leaflet Polygons
#'
#' @description  This function can be used in place of
#'      \code{\link[leaflet]{addPolygons}} and will apply many helper and style
#'      functions for quick consistent maps.
#'
#' @param map a leaflet map
#' @param zcol optional, column in map data to use for filling polygons. Ignored
#'      if \code{fillColor} argument is supplied
#' @param palette a string, palette for filling polygons (default is viridis
#'      for numeric and Okabe-Ito for character/factor), supplied to either
#'      \code{\link[leaflet]{colorNumeric}} or \code{\link[leaflet]{colorFactor}}
#'      for numeric and character/factor respectively
#' @param reverse whether to reverse the \code{palette}, ignored if \code{zcol}
#'      is \code{NULL}
#' @param domain domain used for the \code{palette}, ignored if \code{zcol}
#'      is \code{NULL}. the default \code{domain = NULL} will use all observations
#'      in \code{zcol} as the range
#' @param percent whether to format numeric \code{zcol} palette with percents.
#'      default \code{FALSE}, ignored if \code{zcol} is \code{NULL}.
#' @param legend logical whether or not to create a legend for \code{zcol},
#'      ignored if \code{zcol} is \code{NULL}
#' @param legendposition position of \code{zcol} legend, default 'bottomright',
#'      ignored if \code{zcol} is \code{NULL}
#' @param legendtitle title of \code{zcol} legend, defaults to \code{zcol},
#'      ignored if \code{zcol} is \code{NULL}
#' @param fillColor fill color, string of length 1 or same length as data. For simple
#'      palette generation leave this \code{NULL} and supply \code{zcol} instead,
#'      with optional supporting arguments: palette, percent, reverse, domain.
#'      For more complex palettes, typically a function is created via
#'      \code{leaflet::colorXXXXX()} that is then applied to a column in the data.
#'      Note: you will need to add your own legend if using this argument.
#' @param fillOpacity fill opacity of the polygons, default is \code{0.6}
#' @param stroke whether or not to draw borders on the polygons
#' @param color color of the polygon borders
#' @param weight weight of the polygon borders in pixels
#' @param opacity opacity of the polygon borders
#' @param highlightOptions Options for highlighting the shape on mouse over,
#'      created using \code{leaflet::highlightOptions} function. Default is
#'      \code{highlightOptions(stroke = T, weight = 3, fillOpacity = 0.67)}
#' @param label label shown on hover. For a variable in your data use
#'      \code{~column_name}. Can be HTML, and will automatically
#'      convert to HTML it is not already. See these w3 resources for
#'      \href{https://www.w3schools.com/html/html_styles.asp}{styles} &
#'      \href{https://www.w3schools.com/html/html_formatting.asp}{formatting}
#' @param labelOptions Options for the labels that appear on hover. Defaults to
#'      \code{\link[idgeo]{IDLabelOptions}}
#' @param data the data object from which the argument values are derived; by default,
#'      it is the data object provided to leaflet() or prettyLeaflet() initially,
#'      but can be overridden
#' @param ... any additional parameters supplied to
#'      \code{\link[leaflet]{addPolygons}}, notably "fillColor" and "group"
#'      for fillin and layering the polygons
#'
#' @export
addPrettyPolygons <- function(map, zcol = NULL,
                              palette = NULL,
                              reverse = NULL,
                              domain = NULL,
                              percent = FALSE,
                              legend = !is.null(zcol),
                              legendposition = 'bottomright',
                              legendtitle = zcol,
                              fillColor = NULL,
                              fillOpacity = 0.6,
                              stroke = T,
                              color = 'white',
                              weight = 1, opacity = 1,
                              highlightOptions = NULL,
                              label = NULL,
                              labelOptions = NULL,
                              data = leaflet::getMapData(map),
                              ...){

  x <- NULL

  if(!is.null(fillColor)) {

    # have fill Color, ignore zcol
    addZcolLegend <- function(map){return(map)}

  } else if(!is.null(zcol)) {

    stopifnot(length(zcol)==1)
    stopifnot(inherits(zcol, 'character'))
    stopifnot(zcol %in% names(data))

    x <- data[[zcol]]

    if(is.null(reverse)) reverse <- FALSE

    pal <- idgeo::quickPal(x = x, palette = palette,
                           percent = percent,
                           reverse = reverse,
                           domain = domain)

    # same function quickPal uses for setting domain, need these to be the same
    if(is.null(domain)) domain <- setdomain(x, percent = percent)

    fillColor <- pal(x)

    addZcolLegend <- function(map){
      m <- map |> leaflet::addLegend(position = legendposition,
                                     pal = pal,
                                     values = domain,
                                     title = legendtitle,
                                     labFormat = if(percent){idgeo::labelFormatPercent()
                                     }else{leaflet::labelFormat()}
      )

      return(m)
    }

  } else {

    fillColor <- color #leaflet default
    addZcolLegend <- function(map){return(map)}

  }

  #input_list <- list(...)
  if(is.null(highlightOptions)) highlightOptions <-
      leaflet::highlightOptions(stroke = T, weight = 3, fillOpacity = 0.67)

  if(is.null(labelOptions)) labelOptions <- idgeo::IDLabelOptions()

  if(!is.null(label)){

    #evaluate if formula is given
    labeleval <- try(lazyeval::f_eval(label, data = data),silent = T)
    if(!inherits(labeleval, 'try-error')) label <- labeleval

    # make it html -- it's okay if it's already html
    requireNamespace("htmltools")
    label <- lapply(label, htmltools::HTML)

  } else {

    if(!is.null(x)){

      label <- if(percent){ scales::percent(x)
      } else if(is.numeric(x)){ prettyNum(x,big.mark = ',')
      } else x

      requireNamespace("htmltools")
      label <- lapply(paste0(zcol,': <b>', label, '</b>'),
                      htmltools::HTML)
    }
  }

  map |> leaflet::addPolygons(fillOpacity = fillOpacity,
                              stroke = stroke,
                              color = color,
                              fillColor = fillColor,
                              weight = weight, opacity = opacity,
                              highlightOptions = highlightOptions,
                              label = label,
                              labelOptions = labelOptions,
                              data = data,
                              ...
  ) |> addZcolLegend()

}

#' IDEAS easy button
#'
#' @description  Adds an easy button with the IDEAS logo with info on click.
#'
#' **Important Note**: for the font awesome logo to display correctly either
#' use the \code{\link[idgeo]{prettyLeaflet}}() function when building your map
#' (which will automatically add the fontawesome dependencies), or apply
#' \code{\link[idgeo]{useFontAwesome}}() to your map
#'
#' @param map a leaflet map
#' @param position button position
#' @param extrainfo additional html to include in the box on click
#'
#' @export
addIdeasButton <- function(map, position = 'bottomleft', extrainfo = NULL){

  requireNamespace("htmltools", quietly = TRUE)
  requireNamespace("leaflet.extras", quietly = TRUE)
  requireNamespace('htmlwidgets', quietly = TRUE)

  info.box <- htmltools::HTML(
    paste0(
      htmltools::HTML(
        '<div class="modal fade" id="infobox" role="dialog"><div class="modal-dialog"><!-- Modal content--><div class="modal-content"><div class="modal-header"><button type="button" class="close" data-dismiss="modal">&times;</button>'
      ),

      # Header / Title
      htmltools::HTML('<p>This map has been supplied by IDEAS. Learn more at <a href="http://www.wakeforestid.com" target="_blank">wakeforestid.com</a></p>'),

      htmltools::HTML(
        '</div><div class="modal-body">'
      ),


      # Body

      htmltools::HTML(ifelse(is.null(extrainfo), '',
                             paste0('<p>',extrainfo,'</p><hr>'))),


      htmltools::HTML(paste0('<p style = "text-align:right;"><span style="font-size:larger;">',
                             idstyle::logoTextHTML,'</span>',
                             '<br><span style="font-style:italic;font-size:smaller;">Department of Internal Medicine, Section on Infectious Disease<br>Wake Forest University School of Medicine</span></p>')),

      # Closing divs

      htmltools::HTML('</div><div class="modal-footer"><button type="button" class="btn btn-default" data-dismiss="modal">Close</button></div></div></div></div>')
    )
  )

  m <- map |>
    leaflet::addEasyButton(leaflet::easyButton(
      icon = "fa-head-side-virus",
      title = "Learn more about IDEAS",
      position = position,
      onClick = htmlwidgets::JS("function(btn, map){ $('#infobox').modal('show'); }")
    )) |>
    # Trigger the infobox
    leaflet.extras::addBootstrapDependency() |>
    htmlwidgets::appendContent(info.box)

  return(m)
}
