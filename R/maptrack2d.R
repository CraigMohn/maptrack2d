#' maptrack2d: Draw 2-d map displaying tracks
#'
#'  This package also allows for the creation of maps of a ride or set of rides.
#'  The map area will be slightly larger than the track of the ride, unless you supply
#'  a named list of map lat/long boundaries. If a specific map is not specified from
#'  the list, it will choose the first in the list that covers the track.  The
#'  map is created using the OpenStreetMap package, and there are many choices for
#'  the map format.  Respect the terms of use and do not automate the generation
#'  of maps.
#'
#' @section main call:
#'   \link{draw2d}
#'
#' @import magrittr tibble dplyr stringr ggplot2
#' @import viridis
#' @importFrom grDevices colorRampPalette dev.off heat.colors
#'                       jpeg rainbow tiff
#' @importFrom graphics lines par plot points title
#' @importFrom stats approx median quantile setNames weighted.mean
#' @importFrom scales alpha
#' @importFrom OpenStreetMap openmap projectMercator
#' @importFrom lubridate mday month year second minute hour ymd_hms
#'
#' @name maptrack2d
NULL

###  make the R checker happy
tedious <- utils::globalVariables(c("alphachar","alphahour","distlegend",
                                    "prtchar","prthour","start.hour","start.time",
                                    "startbutton.date","stoplabels","timestamp.s",
                                    "distance.m","segment","timestamp",
                                    "x","xtext.stop","y","xend","xcol",
                                    "verticalMultiplier","color","hjust","label",
                                    "yend","group","timeBeg","timeEnd","pauseSize",
                                    "timelaststop","segbegtime","segendtime",
                                    "movingrun","maxdist","startofstop","sosNA",
                                    "joinseg","subsegment"))


