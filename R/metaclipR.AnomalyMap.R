##     metaclipR.AnomalyMap Construct a directed graph for map metadata encoding
##
##     Copyright (C) 2017 Predictia (http://www.predictia.es)
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

#' @title Directed metadata graph construction for Map graphical products
#' @description Build a directed metadata graph describing a map 
#' @param package package
#' @param version version
#' @param graph i-graph class object containing the input grid whose values are to be mapped
#' @param input.grid Optional. The input grid passed to the plotting function. Some metadata are extracted from 
#' this object if not explicit from the plotting arguments (e.g., the spatial extent of the map).
#' @param epsg.code A character string indicating the EPSG code of the map projection. Default to \code{"4979"}
#' (the most widely used WGS84 proj).
#' @param fun function name. Default to \code{spatialPlot}, from package \code{visualizeR}.
#' @template template_arglistParam
#' @template template_arglist
#' @param withInput The name of the parent node from which the aggregation step hangs.
#' \code{transformeR} indicated in argument \code{fun}.
#' @details This function takes as reference the semantics defined in the Data Source and Transformation ontology
#' defined in the Metaclip Framework (\url{http://www.metaclip.org/}).
#' @family graphical.products
#' @export
#' @importFrom igraph make_empty_graph add_vertices add_edges 
#' @importFrom transformeR getGrid
#' @importFrom utils packageVersion
#' @author D. San Martín, J. Bedia

metaclipR.AnomalyMap <- function(package = "visualizeR",
                                 version = as.character(packageVersion(package)),
                                 graph = NULL,
                                 input.grid = NULL,
                                 fun = "spatialPlot",
                                 arg.list = NULL,
                                 withInput = NULL,
                                 epsg.code = "EPSG:4979") {
    if (is.null(graph)) stop("An input graph is required")
    if (class(graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    if (is.null(withInput)) {
        stop("The 'withInput' property is required: enter the name of the parent node.")
    }
    # if (is.null(graph)) graph <- make_empty_graph()
    map.nodename <- paste("Map", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = map.nodename,
                          label = "Map",
                          className = "gp:Map",
                          description = "A Map class object",
                          attr = list("gp:hasProjection" = as.character(epsg.code),
                                      "gp:hasProductType" = "Seasonal_prediction")
    )
    # SpatialExtent Definition
    if (!is.null(arg.list$xlim)) {
        xmin <- arg.list$xlim[1]
        xmax <- arg.list$xlim[2]
    } else if (is.null(arg.list$xlim) & !is.null(input.grid)) {
        xmin <- getGrid(input.grid)$x[1]
        xmax <- getGrid(input.grid)$x[2]
    } else {
        xmin <- -9999
        xmax <- -9999
    }
    if (!is.null(arg.list$ylim)) {
        ymin <- arg.list$ylim[1]
        ymax <- arg.list$ylim[2]
    } else if (is.null(arg.list$ylim) & !is.null(input.grid)) {
        ymin <- getGrid(input.grid)$y[1]
        ymax <- getGrid(input.grid)$y[2]
    } else {
        ymin <- -9999
        ymax <- -9999
    }
    if (!all(c(xmin, xmax, ymin, ymax) == -9999)) {
        spatextent.nodename <- paste("SpatialExtent", randomName(), sep = ".")
        graph <- add_vertices(graph,
                              nv = 1,
                              name = spatextent.nodename,
                              label = "SpatialExtent",
                              className = "ds:SpatialExtent",
                              description = "SpatialExtent definition",
                              attr = list("ds:xmin" = xmin,
                                          "ds:xmax" = xmax,
                                          "ds:ymin" = ymin,
                                          "ds:ymax" = ymax)
        )
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, map.nodename),
                             getNodeIndexbyName(graph, spatextent.nodename)),
                           label = "gp:hasMapExtent")
    }
    # Layer definition
    maplayer.nodename <- paste("mapLayer", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = maplayer.nodename,
                          label = "MapRasterLayer",
                          className = "gp:MapRaster",
                          description = "Map raster layer definition")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, map.nodename),
                         getNodeIndexbyName(graph, maplayer.nodename)),
                       label = "gp:hasMapLayer")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, maplayer.nodename),
                         getNodeIndexbyName(graph, withInput)),
                       label = "ds:withInput")
    # Additional layers
    if (!is.null(arg.list$backdrop.theme)) {
        descr <- switch(arg.list$backdrop.theme,
                        "coastline" = "Physical coastline vector layer",
                        "countries" = "Political countries vector layer")
        maplayer.nodename <- paste("mapLayer", randomName(), sep = ".")
        graph <- add_vertices(graph,
                              nv = 1,
                              name = maplayer.nodename,
                              label = "MapLinesLayer",
                              className = "gp:MapLines",
                              description = "Map line layer definition",
                              attr = list("gp:hasLayerDescription" = descr))
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, map.nodename),
                             getNodeIndexbyName(graph, maplayer.nodename)),
                           label = "gp:hasMapLayer")
    }
    # Textual annotations
    # Legend
    textannot.nodename <- paste("TextAnnotation", randomName(), sep = ".")
    descr <- "Discrete value-color key"
    graph <- add_vertices(graph,
                          nv = 1,
                          name = textannot.nodename,
                          label = "Legend",
                          className = "gp:TextAnnotation",
                          description = "Definition of text annotations in graphical products",
                          attr = list("gp:hasTextAnnotationType" = descr))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, map.nodename),
                         getNodeIndexbyName(graph, textannot.nodename)),
                       label = "gp:hasTextAnnotation")
    # Title (optional)
    if ("title" %in% names(arg.list)) {
        textannot.nodename <- paste("TextAnnotation", randomName(), sep = ".")
        descr <- arg.list$title
        descr <- gsub("\n","-", descr)
        graph <- add_vertices(graph,
                              nv = 1,
                              name = textannot.nodename,
                              label = "Title",
                              className = "gp:TextAnnotation",
                              description = "Definition of text annotations in graphical products",
                              attr = list("gp:hasTextAnnotationType" = descr))
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, map.nodename),
                             getNodeIndexbyName(graph, textannot.nodename)),
                           label = "gp:hasTextAnnotation")
    }
    # Package/Command/Argument metadata ---------------------------------------
    if ("anomaly.grid" %in% names(arg.list)) arg.list <- arg.list[-grep("anomaly.grid", names(arg.list))]
    graph <- metaclip.graph.Command(graph, package, version, fun, arg.list, origin.node.name = map.nodename)
    return(list("graph" = graph, "parentnodename" = map.nodename))
}
