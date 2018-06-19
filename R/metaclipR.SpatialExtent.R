##     metaclipR.SpatialExtent Construct a directed graph defining a reference SpatialExtent
##
##     Copyright (C) 2018 Predictia (http://www.predictia.es)
##
##     This program is free software: you can redistribute it and/or modify
##     it under the terms of the GNU General Public License as published by
##     the Free Software Foundation, either version 3 of the License, or
##     (at your option) any later version.
## 
##     This program is distributed in the hope that it will be useful,
##     but WITHOUT ANY WARRANTY; without even the implied warranty of
##     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##     GNU General Public License for more details.
## 
##     You should have received a copy of the GNU General Public License
##     along with this program.  If not, see <http://www.gnu.org/licenses/>.


#' @title Set a reference SpatialExtent
#' @description Creates a graph containing a SpatialExtent definition, that can be used later as spatial reference of
#' suubsetting, regridding operations etc. so all steps point to the same reference spatial extent
#' @param xyCoords Grid coordinates (i.e., the \code{$xyCoords} element of a \pkg{climate4R} object)
#' @param region An optional character string designating a known region. See \code{knownClassIndividuals("HorizontalExtent")}
#' for a list of regions defined in the current stable vocabulary version. Default to \code{NULL} and unused. When
#' a known region is used, the spatial extent is already known, but resolution and projections details might be still needed (see the next arguments).
#' Otherwise, the latter data properties are set to \code{NULL}. 
#' @param xmin Optional. When neither \code{obj}, nor \code{RefSpatialExtent} arguments are passed, this is the minimum
#' x coordinate defining the spatial extent
#' @param xmax Same as \code{xmin}, but the maximum
#' @param ymin Same as \code{xmin}, but for the Y coordinates
#' @param ymax Same as \code{ymin}, but for the maximum Y coordinate.
#' @param proj A projection string
#' @param resX Spatial reslution of the grid in the X-coordinates
#' @param resY Same as \code{resX}, but for the Y-coordinates
#' @return A metaclipR list (igraph-class structure + terminal node name)
#' @importFrom igraph make_empty_graph
#' @author J Bedia
#' @export


metaclipR.SpatialExtent <- function(xyCoords, region = NULL,
                                    xmin = NULL, xmax = NULL,
                                    ymin = NULL, ymax = NULL,
                                    proj = NULL,
                                    resX = NULL, resY = NULL) {
    # Comprueba individuos para name
    graph <- make_empty_graph()
    if (!is.null(region)) {
        isKnownRegion <- region %in% suppressMessages(knownClassIndividuals("HorizontalExtent"))
        if (isKnownRegion) {
            spatextent.nodename <- paste0("ds:", region)
            graph <- add_vertices(graph,
                                  nv = 1,
                                  name = spatextent.nodename,
                                  label = paste(region, "region", sep = "_"),
                                  className = "ds:HorizontalExtent",
                                  attr = list("ds:hasProjection" = proj,
                                              "ds:hasHorizontalResX" = resX,
                                              "ds:hasHorizontalResY" = resY))
        } else {
            stop("Invalid region specification\nCheck valid regions using \'knownClassIndividuals(\"SpatialExtent\")\'")
        }
    } else {
        if (!is.null(xyCoords)) {
            xmin <- xyCoords$x[1]
            xmax <- tail(xyCoords$x, 1)
            ymin <- xyCoords$y[1]
            ymax <- tail(xyCoords$y, 1)
            proj <- attr(xyCoords, "projection")
            resX <- attr(xyCoords, "resX")
            resY <- attr(xyCoords, "resY")
        }
        spatextent.nodename <- paste("SpatialExtent", randomName(), sep = ".")
        graph <- add_vertices(graph,
                              nv = 1,
                              name = spatextent.nodename,
                              label = "SpatialExtent",
                              className = "ds:HorizontalExtent",
                              attr = list("ds:xmin" = xmin,
                                          "ds:xmax" = xmax,
                                          "ds:ymin" = ymin,
                                          "ds:ymax" = ymax,
                                          "ds:hasProjection" = proj,
                                          "ds:hasHorizontalResX" = resX,
                                          "ds:hasHorizontalResY" = resY))
    }
    return(list("graph" = graph, "parentnodename" = spatextent.nodename))
}


