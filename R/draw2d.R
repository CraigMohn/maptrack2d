#' plot ride track data on a map
#'
#' \code{draw2d} Plot lat/long data on maps, optionally using colored
#'   dots to indicate speed
#'
#' Create a map displaying the tracks specified.  Tracks may be drawn all in one
#'   specified color, with each separate track drawn in a different color from
#'   a specified palette, or as a series of closely spaced dots (which may
#'   be prodded into appearing as a line through judicious choice of point
#'   size and alpha) with the color varying based on speed and the chosen
#'   palette.
#'
#' @param geodf data frame or tibble containing at least: position_lat.dd,
#'    position_lon.dd,(or lat,lon)(both numeric,decimal degrees),
#'    startbutton.date(int),startbutton.time(int),segment(numeric),
#'    and speed.m.s for speed coloring
#' @param outfile name of output file, extension is either .tiff or .jpg
#' @param maptitle string containing title
#' @param definedmaps list, each named entry is a list of two vectors of
#'    length two, named lat and lon, containing the min and max of the
#'    latitude andf longitude of the map area
#' @param usemap string identifying area from list \eqn{definedmaps} or
#' @param maptype map to use as background (\code{"maptoolkit-topo"} or
#'       \code{"bing"} or \code{"osm"} or \code{"stamen-terrain"} or
#'       \code{"esri-topo"}  or \code{"stamen-watercolor"} or \code{"mapbox"} or
#'       \code{"esri"} or \code{"osm-public-transport"} or \code{"opencyclemap"}
#'       or \code{"apple-iphoto"} or \code{"skobbler"})
#' @param minTiles minimum number of tiles fetched for map, larger is slower
#'    but better quality
#' @param mapsize pixel size of map created
#' @param fine.map if true, use c(7680,4800) for map size
#' @param margin.factor percentage to expand map area beyond limits of
#'    the track in each direction, when autodetermining region to plot
#' @param draw.speed if true, draw track(s) as a series of points whose color
#'    indicates travel speed
#' @param line.color the color to draw the lines of the tracks,
#'    if a palette is specified (\code{"plasma"} or \code{"viridis"} or
#'     \code{"rainbow"} or \code{"heat"} or \code{"red-blue"})is specified,
#'    each track supplied will be assigned a color from that palette
#' @param line.width the width of the line for the tracks
#' @param line.alpha the opacity of the line
#' @param speed.color palette to use to represent speed on plot
#'     (\code{"speedcolors"} or \code{"red-blue-green"} or
#'     \code{"rainbow"} or \code{"plasma"} or \code{"magma"} or
#'     \code{"heat"} )
#' @param speed.alpha opacity of the speed line if \eqn{speed.color} specified
#' @param speed.ptsize size for the symbols used to plot the speed line,
#'    doubled if \eqn{fine.map} is set true.
#' @param speed.pch the character to use plotting the points of the speed line
#' @param jpeg.quality the "quality" of the JPEG image, a percentage. Smaller
#'    values will give more compression but also more degradation of the image
#'
#' @return NULL or plotly object
#'
#' @export
draw2d <- function(geodf,outfile,maptitle,definedmaps,usemap,
                      maptype="osm",minTiles=50,
                      mapsize=c(1600,1200),fine.map=FALSE,margin.factor=0.08,
                      draw.speed=FALSE,
                      line.color="magenta",line.width=3,line.alpha=0.8,
                      speed.color="speedcolors",speed.alpha=0.7,
                      speed.ptsize=6,speed.pch=19,
                      jpeg.quality=90) {
  ##  geodf: a tibble or dataframe containing at least:
        #  position_lat.dd,position_lon.dd,(or lat,lon)(both numeric),
        #  startbutton.time(int),segment(numeric)
  start.hour <- start.time <- startbutton.date <-  NULL
  stoplabels <- timestamp.s <- NULL
  if (substr(outfile,nchar(outfile)-4,nchar(outfile))==".tiff") {
    outfiletype <- "tiff"
  } else if (substr(outfile,nchar(outfile)-3,nchar(outfile))==".jpg") {
    outfiletype <- "jpeg"
  } else if (outfile=="none") {
    outfiletype <- "none"
  } else {
    stop("invalid output filetype")
  }
  if (fine.map) mapsize <- c(7680,4800)
  if (fine.map) speed.ptsize <- 2*speed.ptsize

  colnames(geodf) <- gsub("position_lat.dd","lat",colnames(geodf))
  colnames(geodf) <- gsub("position_lon.dd","lon",colnames(geodf))
  geodf <- geodf[!is.na(geodf$lat),]
  if (nrow(geodf)> 0) {
    geodf$start.time <-
         dateTimeStr(geodf$startbutton.date,geodf$startbutton.time)
    lat.max <- max(geodf$lat)
    lat.min <- min(geodf$lat)
    lon.max <- max(geodf$lon)
    lon.min <- min(geodf$lon)
    # expand the map area beyond the limits of the tracks
    marginfactor <- margin.factor
    if (!missing(definedmaps)) {
      if (!missing(usemap)) {
        u.map <- definedmaps[[usemap]]
        if (is.null(u.map)) {
          warning(paste0("Exactly where did you define map ",usemap,"??"))
          return(NULL)
        }
        cat("\nUsing defined map area ",usemap)
        lat.min <- u.map[["lat"]][1]
        lat.max <- u.map[["lat"]][2]
        lon.min <- u.map[["lon"]][1]
        lon.max <- u.map[["lon"]][2]
      } else {
        for (i in seq_along(definedmaps)) {
          u.map <- definedmaps[[i]]
          if (lat.min >= u.map[["lat"]][1] & lat.max <= u.map[["lat"]][2] &
              lon.min >= u.map[["lon"]][1] & lon.max <= u.map[["lon"]][2]) {
            cat("\nusing defined map area ",names(definedmaps)[[i]])
            lat.min <- u.map[["lat"]][1]
            lat.max <- u.map[["lat"]][2]
            lon.min <- u.map[["lon"]][1]
            lon.max <- u.map[["lon"]][2]
            break
          }
        }
      }
      marginfactor <- 0.0
    }

    latwide <- lat.max - lat.min
    lonwide <- lon.max - lon.min
    map.lat.max.dd <- lat.max+latwide*marginfactor
    if (!missing(maptitle)) map.lat.max.dd <- lat.max+latwide*0.05
    map.lat.min.dd <- lat.min-latwide*marginfactor
    map.lon.max.dd <- lon.max+lonwide*marginfactor
    map.lon.min.dd <- lon.min-lonwide*marginfactor
    if (outfiletype != "none") {
      if (min(lat.max-lat.min,lon.max-lon.min) > .005){
        map <- OpenStreetMap::openmap(c(map.lat.max.dd, map.lon.min.dd),
                                      c(map.lat.min.dd, map.lon.max.dd),
                                      type=maptype,minNumTiles=minTiles)
        #native mercator-for longlat add:
        #                map <- openproj(map,projection="+proj=longlat")
      } else {
        cat("\n",outfile," not created, map too small")
        return(NULL)
      }
      map.lat.min <- map$bbox[["p2"]][2] * (180 / (2 ^ 31))
      map.lat.max <- map$bbox[["p1"]][2] * (180 / (2 ^ 31))
      map.lon.min <- map$bbox[["p1"]][1] * (180 / (2 ^ 31))
      map.lon.max <- map$bbox[["p2"]][1] * (180 / (2 ^ 31))
      aspectcorrect <- 1  #  using mercator, better than latlon at northern lats
      aspectratio <- (map.lon.max - map.lon.min)/(map.lat.max-map.lat.min)
      if (aspectratio > mapsize[1]/mapsize[2]) {
        mapwidth <- mapsize[1]
        mapheight <- mapsize[1]/aspectratio
      } else {
        mapwidth <- mapsize[2]*aspectratio
        mapheight <- mapsize[2]
      }
    }

    #  expand limits so any segments that have their middle cut out will
    #        be wrongly rendered, but off-map
    #    (except perhaps near corners, not worth fixing now, maybe never)
    mapdf <- geodf[geodf$lon>=map.lon.min.dd-.01 &
                   geodf$lon<=map.lon.max.dd+.01 &
                   geodf$lat>=map.lat.min.dd-.01 &
                   geodf$lat<=map.lat.max.dd+.01,]
    trackstarts <- unique(mapdf$start.time)

    if (nrow(mapdf) > 0) {
      if (draw.speed) {
        if (speed.color=="plasma") {
          spdcolors <- rev(plasma(101))
        } else if (speed.color=="magma") {
          spdcolors <- rev(magma(101))
        } else if (speed.color=="heat") {
          spdcolors <- rev(heat.colors(101))
        } else if (speed.color=="rainbow") {
          spdcolors <- (rainbow(101,start=0.15,end=1))
        } else if (speed.color=="red-blue-green") {
          spdcolors <- colorRampPalette(c("red","blue","green"))(101)
        } else {
          spdcolors <- colorRampPalette(c("red","orange","cornflowerblue",
                                          "dodgerblue","blue","darkorchid",
                                          "purple","magenta"))(101)
        }
        speed <- mapdf$speed.m.s*2.23694
        speed[speed>40] <- 40
        speed[speed<3] <- 3
        mapdf$colorvec <- spdcolors[floor(100*(speed - 3)/37) + 1]
      } else {
        if (line.color=="plasma") {
          mapcvec <- viridis::plasma(length(trackstarts),begin=0.0,end=0.7)
        } else if (line.color=="viridis") {
          mapcvec <- viridis::viridis(length(trackstarts),begin=0.1,end=0.9)
        } else if (line.color=="rainbow") {
          mapcvec <- rainbow(length(trackstarts),start=0.2,end=0.9)
        } else if (line.color=="heat") {
          mapcvec <- heat.colors(length(trackstarts))
        } else if (line.color=="red-blue") {
          mapcvec <- colorRampPalette(c("red","blue"))(101)
        } else {
          mapcvec <- rep(line.color,length(trackstarts))
        }
        mapdf$colorvec <- mapcvec[match(mapdf$start.time, trackstarts)]
      }
    }

    if (outfiletype!="none") {
      cat("\noutfile=",outfile)
      if (outfiletype=="jpeg") {
        jpeg(outfile, width = mapwidth,height=mapheight,quality=jpeg.quality)
      } else if (outfiletype=="tiff") {
        tiff(outfile, width = mapwidth, height = mapheight,
             type="cairo",compression="zip+p")
      }
      par(mar = rep(0,4))
      plot(map)
      if (!missing(maptitle)) title(main=as.character(maptitle),
           cex.main=2*mapheight/800,col="gray57",line=-3*(mapheight/800))
      if (nrow(mapdf > 0)) {
        if (!draw.speed) {
          for (trkstarttime in trackstarts) {
            draw.color <- mapdf$colorvec[which(trackstarts == trkstarttime)]
            for (seg in unique(mapdf[mapdf$start.time==trkstarttime,]$segment)) {
              use = mapdf$start.time==trkstarttime & mapdf$segment==seg &
                   (lead_one(mapdf$segment)==mapdf$segment |
                    lag_one(mapdf$segment)==mapdf$segment) # can't do 1 pt lines
              if(sum(use)>0) {
                temp <-
                   OpenStreetMap::projectMercator(mapdf$lat[use],mapdf$lon[use])
                lines(temp[,1], temp[,2],type = "l",
                      col = scales::alpha(mapdf$colorvec[use][[1]], line.alpha),
                      lwd = line.width)
              }
            }
          }
        } else {
          temp <- OpenStreetMap::projectMercator(mapdf$lat, mapdf$lon)
          points(temp[,1],
                 temp[,2],
                 pch=speed.pch,
                 col=scales::alpha(mapdf$colorvec,speed.alpha),
                 lwd=speed.ptsize)
        }
      }
      dev.off()
    }
  }
}
