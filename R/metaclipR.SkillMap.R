##     metaclipR.SkillMap Construct a directed graph for skill map metadata encoding
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

#' @title Directed metadata graph construction for skill Map graphical products
#' @description Build a directed metadata graph describing a skill map 
#' @param package package
#' @param version version
#' @param graph metaclipR output object containing the input grid whose values are to be mapped
#' @param input.grid Optional. The input grid passed to the plotting function. Some metadata are extracted from 
#' this object if not explicit from the plotting arguments (e.g., the spatial extent of the map).
#' @param epsg.code A character string indicating the EPSG code of the map projection. Default to \code{"4979"}
#' (the most widely used WGS84 proj).
#' @param fun function name. Default to \code{spatialPlot}, from package \code{visualizeR}.
#' @template template_arglistParam
#' @template template_arglist
#' @param StipplingInputGraph Currently unused. The metaclipR output containing the step from which the stippling is derived (default to \code{NULL},
#'  and no stippling is indicated)
#' @details This function takes as reference the semantics defined in the Data Source and Transformation ontology
#' defined in the Metaclip Framework (\url{http://www.metaclip.org/}).
#' 
#' @references 
#' \href{https://docs.google.com/presentation/d/1Bn7M7IIFVvOoKG7YymXkDnNH4vJOqO6xmimeUI6Uxc0/view#slide=id.g23b883c69b_0_194}{Visual schema of the graphical product ontology}
#' \href{http://www.meteo.unican.es/en/climate4r}{Climate4R page at University of Cantabria}
#' @family graphical.products
#' @export
#' @importFrom igraph make_empty_graph add_vertices add_edges 
#' @importFrom transformeR getGrid
#' @author D. San Martín, J. Bedia

metaclipR.SkillMap <- function(graph, 
                               package = "visualizeR",
                               version = "1.1.1",
                               input.grid = NULL,
                               fun = "skillMap",
                               arg.list = NULL,
                               StipplingInputGraph = NULL,
                               epsg.code = "EPSG:4979") {
    if (class(graph$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    withInput <- graph$parentnodename
    graph <- graph$graph
    epsg.code <- as.character(epsg.code)
    map.nodename <- paste("Map", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = map.nodename,
                          label = "Map",
                          className = "go:Map",
                          description = "A Map class object",
                          attr = list("go:hasProjection" = epsg.code,
                                      "go:hasProductType" = "EQC Product")
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
                           label = "go:hasMapExtent")
    }
    # Layer definition
    maplayer.nodename <- paste("mapLayer", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = maplayer.nodename,
                          label = "MapRasterLayer",
                          className = "go:MapRaster",
                          description = "Map raster layer definition")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, map.nodename),
                         getNodeIndexbyName(graph, maplayer.nodename)),
                       label = "go:hasMapLayer")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, withInput),
                         getNodeIndexbyName(graph, maplayer.nodename)),
                       label = "go:hadGraphicalRepresentation")
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
                          attr = list("go:hasTextAnnotationType" = descr))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, map.nodename),
                         getNodeIndexbyName(graph, textannot.nodename)),
                       label = "go:hasTextLayer")
    # Title (optional)
    if ("title" %in% names(arg.list)) {
        textannot.nodename <- paste("TextAnnotation", randomName(), sep = ".")
        descr <- arg.list$title
        descr <- gsub("\n","-", descr)
        graph <- add_vertices(graph,
                              nv = 1,
                              name = textannot.nodename,
                              label = "Title",
                              className = "go:TextLayer",
                              attr = list("go:hasTextAnnotationType" = descr))
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, map.nodename),
                             getNodeIndexbyName(graph, textannot.nodename)),
                           label = "go:hasTextLayer")
    }
    # Point Layer (stippling)
    if ("stippling" %in% names(arg.list)) {
        if (!is.null(arg.list$"stippling"$threshold)) {
            layer.nodename <- paste("MapLayer", randomName(), sep = ".")
            descr <- paste(paste0("Map Stippling->threshold=", round(arg.list$stippling$threshold,2)),
                           paste0("condition=", arg.list$stippling$condition),
                           sep = "|")
            graph <- add_vertices(graph,
                                  nv = 1,
                                  name = layer.nodename,
                                  label = "Stippling",
                                  className = "go:MapPoints",
                                  description = "Definition of point layers in map products",
                                  attr = list("go:hasLayerDescription" = descr))
            graph <- add_edges(graph,
                               c(getNodeIndexbyName(graph, map.nodename),
                                 getNodeIndexbyName(graph, layer.nodename)),
                               label = "go:hasMapLayer")
            graph <- add_edges(graph,
                               c(getNodeIndexbyName(graph, withInput),
                                 getNodeIndexbyName(graph, layer.nodename)),
                               label = "go:hadGraphicalRepresentation")
            # Add withInput class
            # To be implemented
            # Add hasUncertainty class
            uncert.nodename <- paste("Uncertainty", randomName(), sep = ".")
            graph <- add_vertices(graph,
                                  nv = 1,
                                  name = uncert.nodename,
                                  label = "Uncertainty",
                                  className = "go:SamplingUncertainty",
                                  description = "Uncertainty communication")
            graph <- add_edges(graph,
                               c(getNodeIndexbyName(graph, layer.nodename),
                                 getNodeIndexbyName(graph, uncert.nodename)),
                               label = "go:hasUncertainty")
        }
    }
    # Package/Command/Argument metadata ---------------------------------------
    if ("easyVeriGrid" %in% names(arg.list)) arg.list <- arg.list[-grep("easyVeriGrid", names(arg.list))]
    if (!is.null(arg.list$title)) arg.list$title <- gsub("\n","|", arg.list$title)
    graph <- metaclip.graph.Command(graph, package, version, fun, arg.list, origin.node.name = map.nodename)
    return(list("graph" = graph, "parentnodename" = map.nodename))
}
