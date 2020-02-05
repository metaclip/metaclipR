##     metaclipR.RectangularGrid Construct a directed graph defining a RectagularGrid
##
##     Copyright (C) 2020 Santander Meteorology Group (http://www.meteo.unican.es)
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


#' @title Set a reference RectangularGrid
#' @description Creates a graph containing a RectangularGrid definition, 
#' that can be used later as spatial reference for regridding operations etc. so all steps point to the same reference grid
#' @param xmin Optional. The minimum x coordinate defining the spatial extent
#' @param xmax Same as \code{xmin}, but the maximum
#' @param ymin Same as \code{xmin}, but for the Y coordinates
#' @param ymax Same as \code{ymin}, but for the maximum Y coordinate.
#' @param resX Required. Spatial resolution of the grid in the X-coordinates
#' @param resY Required. Same as \code{resX}, but for the Y-coordinates
#' @param dc.description Default to \code{NULL} and unused. Otherwise, this is a character string that will be appendend as a
#'  "dc:description" annotation to the ds:RectangularGrid node.
#' @return A metaclipR object (igraph-class structure + terminal node name)
#' @importFrom igraph make_empty_graph
#' @author J Bedia
#' @export

metaclipR.RectangularGrid <- function(resX,
                                      resY,
                                      xmin = NULL,
                                      xmax = NULL,
                                      ymin = NULL,
                                      ymax = NULL,
                                      dc.description = NULL) {
    graph <- make_empty_graph()
    grid.nodename <- paste("RectangularGrid", randomName(), sep = ".")
    if (is.null(dc.description)) {
        graph <- add_vertices(graph,
                              nv = 1,
                              name = grid.nodename,
                              label = "Rectangular Grid",
                              className = "ds:RectangularGrid",
                              attr = list("ds:xmin" = xmin,
                                          "ds:xmax" = xmax,
                                          "ds:ymin" = ymin,
                                          "ds:ymax" = ymax,
                                          "ds:hasHorizontalResX" = resX,
                                          "ds:hasHorizontalResY" = resY))
    } else {
        graph <- add_vertices(graph,
                              nv = 1,
                              name = grid.nodename,
                              label = "Rectangular Grid",
                              className = "ds:RectangularGrid",
                              attr = list("ds:xmin" = xmin,
                                          "ds:xmax" = xmax,
                                          "ds:ymin" = ymin,
                                          "ds:ymax" = ymax,
                                          "ds:hasHorizontalResX" = resX,
                                          "ds:hasHorizontalResY" = resY,
                                          "dc:description" = dc.description))
        
    }
    return(list("graph" = graph, "parentnodename" = grid.nodename))
}


