##     metaclipR.spatialPlot Construct a directed graph for skill map metadata encoding
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

#' @title Directed metadata graph construction for spatialPlot outputs
#' @description Build a directed metadata graph describing a spatialPlot in visualizeR
#' @param package package
#' @param version version
#' @param graph i-graph class object containing the input grid whose values are to be mapped
#' @param type The type of input grid passed to \code{spatialPlot}. Default to \code{"grid"}. The other possible choice is \code{"stations"},
#' for station datasets. Whie the first is mapped to the go:MapRaster class, the latter is assigned to go:MapPoints.
#' @param input.grid Optional. The input grid passed to the plotting function. Some metadata are extracted from 
#' this object if not explicit from the plotting arguments (e.g., the spatial extent of the map).
#' @param epsg.code A character string indicating the EPSG code of the map projection. Default to \code{"4979"}
# (the most widely used WGS84 proj).
#' @param fun function name. Default to \code{spatialPlot}, from package \code{visualizeR}.
#' @param arg.list Argument list (A named list in key-value format).
#' @param stipplingInputGraph Currently unused. The graph containing the step from which the stippling is derived (default to \code{NULL},
#'  and no stippling is indicated)
#' @param isBranchStep Logical flag. For internal usage only, and rarely needed. It indicates wheter the GraphicalOutput is the
#'  new terminal node of the graph representation (\code{FALSE} the default), or it is is a secondary branch, and the last terminal node
#'  should be preserved as the link for the next step.
#' @details 
#' 
#' It is important to note that the resulting output of this function will reatin as parent node the last step before plotting,
#'  so intermediate plots within the data workflow can be recorded in the provenance track without affecting
#'  subsequent operations.
#' 
#' This function takes as reference the semantics defined in the Data Source and Transformation ontology
#' defined in the Metaclip Framework (\url{http://www.metaclip.org}).
#' @family graphical.products
#' @export
#' @importFrom igraph make_empty_graph add_vertices add_edges 
#' @importFrom transformeR getGrid
#' @author D. San Martín, J. Bedia

metaclipR.SpatialPlot <- function(package = "visualizeR",
                                  version = "1.1.1",
                                  graph,
                                  input.grid = NULL,
                                  type = "grid",
                                  fun = "spatialPlot",
                                  arg.list = NULL,
                                  stipplingInputGraph = NULL,
                                  epsg.code = "EPSG:4329",
                                  isBranchStep = FALSE) {
    if (class(graph$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    type <- match.arg(type, c("grid", "stations"))
    stopifnot(is.logical(isBranchStep))
    withInput <- graph$parentnodename
    graph <- graph$graph
    map.nodename <- paste("Map", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = map.nodename,
                          label = "Map",
                          className = "go:Map",
                          description = "A Map class object",
                          attr = list("go:hasProjection" = as.character(epsg.code)))
    # graph <- add_edges(graph,
    #                    c(getNodeIndexbyName(graph, withInput),
    #                      getNodeIndexbyName(graph, map.nodename)),
    #                    label = "go:hadGraphicalRepresentation")
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
                              attr = list("ds:xmin" = xmin,
                                          "ds:xmax" = xmax,
                                          "ds:ymin" = ymin,
                                          "ds:ymax" = ymax)
        )
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, map.nodename),
                             getNodeIndexbyName(graph, spatextent.nodename)),
                           label = "go:hasMapExtent")
    }
    # Layer definition
    n.mem <- ifelse(is.null(input.grid), 1, getShape(input.grid, "member"))
    if (is.na(n.mem)) n.mem <- 1
    classname <- ifelse(type == "grid", "go:MapRaster", "go:MapPoints")
    for (i in 1:n.mem) {
        maplayer.nodename <- paste("mapLayer", randomName(), i, sep = ".")
        graph <- add_vertices(graph,
                              nv = 1,
                              name = maplayer.nodename,
                              label = "MapLayer",
                              className = classname)
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, withInput),
                             getNodeIndexbyName(graph, maplayer.nodename)),
                           label = "go:hadGraphicalRepresentation")
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, map.nodename),
                             getNodeIndexbyName(graph, maplayer.nodename)),
                           label = "go:hasMapLayer")
    }
    # Additional layers
    if (!is.null(arg.list$backdrop.theme)) {
        descr <- switch(arg.list$backdrop.theme,
                        "coastline" = "Physical coastline vector lines layer",
                        "countries" = "Political countries vector lines layer")
        maplayer.nodename <- paste("mapLayer", randomName(), sep = ".")
        graph <- add_vertices(graph,
                              nv = 1,
                              name = maplayer.nodename,
                              label = "MapLinesLayer",
                              className = "go:MapLines",
                              attr = list("go:hasLayerDescription" = descr))
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, map.nodename),
                             getNodeIndexbyName(graph, maplayer.nodename)),
                           label = "go:hasMapLayer")
    }
    # Textual annotations
    # Legend
    textannot.nodename <- paste("TextLayer", randomName(), sep = ".")
    descr <- "Discrete value-color key"
    graph <- add_vertices(graph,
                          nv = 1,
                          name = textannot.nodename,
                          label = "Legend",
                          className = "go:TextLayer",
                          description = "Definition of text annotations in graphical products",
                          attr = list("go:hasTextAnnotationType" = descr))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, map.nodename),
                         getNodeIndexbyName(graph, textannot.nodename)),
                       label = "go:hasTextLayer")
    # Title (optional)
    if ("title" %in% names(arg.list)) {
        textannot.nodename <- paste("TextLayer", randomName(), sep = ".")
        descr <- arg.list$title
        descr <- gsub("\n","-", descr)
        graph <- add_vertices(graph,
                              nv = 1,
                              name = textannot.nodename,
                              label = "Title",
                              className = "go:TextLayer",
                              description = "Definition of text annotations in graphical products",
                              attr = list("go:hasTextAnnotationType" = descr))
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, map.nodename),
                             getNodeIndexbyName(graph, textannot.nodename)),
                           label = "go:hasTextLayer")
    }
    # # Point Layer (stippling)
    # if ("stippling" %in% names(arg.list)) {
    #     if (!is.null(arg.list$"stippling"$threshold)) {
    #         layer.nodename <- paste("MapLayer", randomName(), sep = ".")
    #         descr <- paste(paste0("Map Stippling->threshold=", round(arg.list$stippling$threshold,2)),
    #                        paste0("condition=", arg.list$stippling$condition),
    #                        sep = "|")
    #         graph <- add_vertices(graph,
    #                               nv = 1,
    #                               name = layer.nodename,
    #                               label = "Stippling",
    #                               className = "go:MapPoints",
    #                               description = "Definition of point layers in map products",
    #                               attr = list("go:hasLayerDescription" = descr))
    #         graph <- add_edges(graph,
    #                            c(getNodeIndexbyName(graph, map.nodename),
    #                              getNodeIndexbyName(graph, layer.nodename)),
    #                            label = "go:hasMapLayer")
    #         # Add withInput class
    #         # To be implemented
    #         # Add hasUncertainty class
    #         uncert.nodename <- paste("Uncertainty", randomName(), sep = ".")
    #         graph <- add_vertices(graph,
    #                               nv = 1,
    #                               name = uncert.nodename,
    #                               label = "Uncertainty",
    #                               className = "go:SamplingUncertainty",
    #                               description = "Uncertainty communication")
    #         graph <- add_edges(graph,
    #                            c(getNodeIndexbyName(graph, layer.nodename),
    #                              getNodeIndexbyName(graph, uncert.nodename)),
    #                            label = "go:hasUncertainty")
    #     }
    # }
    # Package/Command/Argument metadata ---------------------------------------
    if ("grid" %in% names(arg.list)) arg.list <- arg.list[-grep("grid", names(arg.list))]
    if (!is.null(arg.list$title)) arg.list$title <- gsub("\n","|", arg.list$title)
    if (!is.null(arg.list$main)) arg.list$main <- gsub("\n","|", arg.list$main)
    graph <- metaclip.graph.Command(graph, package, version, fun, arg.list, origin.node.name = map.nodename)
    parentnodename <- ifelse(isBranchStep, withInput, map.nodename)
    return(list("graph" = graph, "parentnodename" = parentnodename))
}
