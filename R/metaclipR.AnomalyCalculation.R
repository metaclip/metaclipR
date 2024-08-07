##     metaclipR.AnomalyCalculation Construct a directed graph for encoding anomaly transformations
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

#' @title Directed metadata graph construction for Anomaly transformations  
#' @description Build a directed metadata graph describing an anomaly Transformation on a 
#' climate4R grid 
#' @param package package
#' @param version version
#' @param fun function name. Unused (set to \code{"localScaling"})
 #' @param dc.description Default to \code{NULL} and unused. Otherwise, this is a character string that will be appendend as a
#'  "dc:description" annotation to the ds:AnomalyCalculation-class node.
#' @template template_arglistParam
#' @template template_arglist
#' @param graph An output from a previous \pkg{metaclipR} function containing a list with the i-graph class object containing
#'  the input grid whose anomaly is to be computed, plus the terminal node from which the Anomaly Step will hang
#' @param referenceGraph An output from a previous \pkg{metaclipR} function containing a list with the i-graph class object containing the reference Transformation-class object
#' used as base to compute the climatology, plus the name of its terminal node
#' @details This function takes as reference the semantics defined in the Data Source and Transformation ontology
#' defined in the Metaclip Framework (\url{http://www.metaclip.org}).
#' @family transformation
#' @export
#' @importFrom igraph add_vertices add_edges 
#' @author D. San Martín, J. Bedia


metaclipR.AnomalyCalculation <- function(graph,
                                         package = "transformeR",
                                         version = "1.4.1",
                                         fun = "scaleGrid",
                                         arg.list = NULL,
                                         referenceGraph = NULL,
                                         dc.description = NULL) {
    if (class(graph$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    withInput <- graph$parentnodename
    graph <- graph$graph
    if (is.null(withInput)) {
        stop("The 'withInput' property is required: enter the name of the parent node.")
    }
    if (!"base" %in% names(arg.list)) stop("The 'base' argument is expected in the argument list")
    orig.nodes.command <- c()
    # graph <- metaclipR.DatasetSubset(graph = graph, output = output, disable.command = TRUE)
    anom.nodename <- paste("Anomaly", randomName(), sep = ".")
    orig.nodes.command <- c(orig.nodes.command, anom.nodename)
    if (is.null(dc.description)) {
        graph <- add_vertices(graph,
                              nv = 1,
                              name = anom.nodename,
                              label = "Anomaly",
                              className = "ds:Anomaly",
                              attr = list("ds:hasTimeFrame" = arg.list$time.frame))    
    } else {
        graph <- add_vertices(graph,
                              nv = 1,
                              name = anom.nodename,
                              label = "Anomaly",
                              className = "ds:Anomaly",
                              attr = list("ds:hasTimeFrame" = arg.list$time.frame,
                                          "dc:description" = dc.description))
    }
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, withInput),
                         getNodeIndexbyName(graph, anom.nodename)),
                       label = "ds:hadAnomalyCalculation")
    if (is.null(arg.list$base)) {
        # We add a Climatology transformation, which is the reference:
        if (is.null(arg.list$clim.fun$FUN)) {
            # stop("The 'clim.fun' argument was not found in the argument list:\nRequired to indicate the cell method of the climatological reference")
            arg.list$clim.fun$FUN <- "mean"
        }
        clim.base.nodename <- paste("climatology", randomName(), sep = ".")
        cellme <- paste(deparse(arg.list$clim.fun$FUN), collapse = "")
        cellme <- gsub("\"","'", cellme)
        graph <- add_vertices(graph,
                              nv = 1,
                              name = clim.base.nodename,
                              label = "Climatology",
                              className = "ds:Climatology",
                              attr = list("hasCellMethod" = cellme))
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, anom.nodename),
                             getNodeIndexbyName(graph, clim.base.nodename)),
                           label = "ds:withReference")
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, withInput),
                             getNodeIndexbyName(graph, clim.base.nodename)),
                           label = "ds:hadClimatology")
        orig.nodes.command <- c(orig.nodes.command, clim.base.nodename)
    } else {
        if (is.null(referenceGraph) & !is.null(arg.list$base)) {
            stop("A second graph containing the reference for anomaly calculation is required")
        }
        if (!is.null(referenceGraph)) {
            if (class(referenceGraph$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
            # Graphs 1 and 2 are joined ----------------------
            uniongraph <- my_union_graph(graph, referenceGraph$graph)
            graph <- add_edges(uniongraph,
                               c(getNodeIndexbyName(uniongraph, anom.nodename),
                                 getNodeIndexbyName(uniongraph, referenceGraph$parentnodename)),
                               label = "ds:withReference")
        }
    }
    ## Package/Command/Argument metadata ---------------------
    if ("grid" %in% (names(arg.list))) arg.list <- arg.list[-grep("grid", names(arg.list))]
    if ("base" %in% (names(arg.list))) arg.list <- arg.list[-grep("base", names(arg.list))]
    if ("ref" %in% (names(arg.list))) arg.list <- arg.list[-grep("ref", names(arg.list))]
    graph <- metaclip.graph.Command(graph = graph,
                                    package = package,
                                    version = version,
                                    fun = fun,
                                    arg.list = arg.list,
                                    origin.node.name = orig.nodes.command)
    return(list("graph" = graph, "parentnodename" = anom.nodename))
}



#' @title Directed metadata graph construction for Anomaly transformations  
#' @description Build a directed metadata graph describing an anomaly Transformation
#' @param package package
#' @param version version
#' @param fun function name. Unused (set to \code{"scaleGrid"})
#' @param time.frame Time frame considered for climatological reference calculation
#' @param clim.cell.method Optional. cell method for climatology calculation. Default to \code{"mean"}.
#' @param disable.command Better not to touch. For internal usage only (used to re-use most of the code 
#' in other functions, but skipping command tracking)
#' @template template_arglistParam
#' @template template_arglist
#' @param graph An output from a previous \pkg{metaclipR} function containing a list with the i-graph class object containing
#'  the input grid whose anomaly is to be computed, plus the terminal node from which the Anomaly Step will hang
#' @param referenceGraph An output from a previous \pkg{metaclipR} function containing a list with the i-graph class object containing the reference Transformation-class object
#' used as base to compute the climatology, plus the name of its terminal node
#' @export

metaclipR.Anomaly <- function(graph,
                              package = "transformeR",
                              version = "1.4.4",
                              fun = "scaleGrid",
                              time.frame = "seasonal",
                              arg.list = NULL,
                              referenceGraph = NULL,
                              clim.cell.method = "mean",
                              disable.command = FALSE) {
    if (class(graph$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    time.frame <- match.arg(time.frame, choices = c("monthly", "annual", "seasonal"))
    withInput <- graph$parentnodename
    graph <- graph$graph
    if (is.null(withInput)) {
        stop("The 'withInput' property is required: enter the name of the parent node.")
    }
    stopifnot(is.logical(disable.command))
    orig.nodes.command <- c()
    anom.nodename <- paste("Anomaly", randomName(), sep = ".")
    orig.nodes.command <- c(orig.nodes.command, anom.nodename)
    graph <- add_vertices(graph,
                          nv = 1,
                          name = anom.nodename,
                          label = "Anomaly",
                          className = "ds:Anomaly",
                          attr = list("ds:hasTimeFrame" = time.frame))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, withInput),
                         getNodeIndexbyName(graph, anom.nodename)),
                       label = "ds:hadAnomalyCalculation")
    if (!is.null(referenceGraph)) {
        if (class(referenceGraph$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
        # Graphs 1 and 2 are joined ----------------------
        uniongraph <- my_union_graph(graph, referenceGraph$graph)
        graph <- add_edges(uniongraph,
                           c(getNodeIndexbyName(uniongraph, anom.nodename),
                             getNodeIndexbyName(uniongraph, referenceGraph$parentnodename)),
                           label = "ds:withReference")
    } else {
        clim.base.nodename <- paste("climatology", randomName(), sep = ".")
        graph <- add_vertices(graph,
                              nv = 1,
                              name = clim.base.nodename,
                              label = "Climatology",
                              className = "ds:Climatology",
                              attr = list("hasCellMethod" = "mean"))
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, anom.nodename),
                             getNodeIndexbyName(graph, clim.base.nodename)),
                           label = "ds:withReference")
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, withInput),
                             getNodeIndexbyName(graph, clim.base.nodename)),
                           label = "ds:hadClimatology")
        orig.nodes.command <- c(orig.nodes.command, clim.base.nodename)
    }
    ## Package/Command/Argument metadata ---------------------
    if (!disable.command) {
        if ("grid" %in% (names(arg.list))) arg.list <- arg.list[-grep("grid", names(arg.list))]
        if ("base" %in% (names(arg.list))) arg.list <- arg.list[-grep("base", names(arg.list))]
        if ("ref" %in% (names(arg.list))) arg.list <- arg.list[-grep("ref", names(arg.list))]
        graph <- metaclip.graph.Command(graph = graph,
                                        package = package,
                                        version = version,
                                        fun = fun,
                                        arg.list = arg.list,
                                        origin.node.name = orig.nodes.command)
    }
    return(list("graph" = graph, "parentnodename" = anom.nodename))
}


