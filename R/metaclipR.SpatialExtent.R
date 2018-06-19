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
#' for a list of regions defined in the current stable vocabulary version. Default to \code{NULL} and unused.
#' @return A metaclipR list (igraph-class structure + terminal node name)
#' @importFrom igraph make_empty_graph
#' @author J Bedia
#' @export


metaclipR.SpatialExtent <- function(xyCoords, region = NULL) {
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
                                  className = "ds:HorizontalExtent")
        } else {
            stop("Invalid region specification\nCheck valid regions using \'knownClassIndividuals(\"SpatialExtent\")\'")
        }
    } else {
        spatextent.nodename <- paste("SpatialExtent", randomName(), sep = ".")
        graph <- add_vertices(graph,
                              nv = 1,
                              name = spatextent.nodename,
                              label = "SpatialExtent",
                              className = "ds:HorizontalExtent",
                              attr = list("ds:xmin" = xyCoords$x[1],
                                          "ds:xmax" = tail(xyCoords$x, 1),
                                          "ds:ymin" = xyCoords$y[1],
                                          "ds:ymax" = tail(xyCoords$y, 1),
                                          "ds:hasProjection" = attr(xyCoords, "projection"),
                                          "ds:hasHorizontalResX" = attr(xyCoords, "resX"),
                                          "ds:hasHorizontalResY" = attr(xyCoords, "resY")))
    }
    return(list("graph" = graph, "parentnodename" = spatextent.nodename))
}


