##     metaclipR.Climatology Construct a directed graph for Climatology metadata encoding
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

#' @title Directed metadata graph construction for Climatology transformations 
#' @description Build a directed metadata graph describing a Transformation on a 
#' climate4R grid that is a Climatology
#' @param package package
#' @param graph A previous metaclipR data structure from which the current step follows
#' @param version version
#' @param fun function name. Unused (set to \code{"climatology"})
#' @details This function takes as reference the semantics defined in the Data Source and Transformation ontology
#' defined in the Metaclip Framework (\url{http://www.metaclip.org}).
#' @template template_arglistParam
#' @template template_arglist
#' @references 
#' \href{http://www.meteo.unican.es/en/climate4r}{Climate4R page at University of Cantabria}
#' @family transformation
#' @return A named list with the updated graph in element \code{"graph"} and the parent node name (\code{"parentnodename"}),
#' needed for linking subsequent operations.
#' @export
#' @importFrom igraph make_empty_graph add_vertices add_edges 
#' @author D. San Martín, J. Bedia

metaclipR.Climatology <- function(package = "transformeR",
                                  version = "1.3.2",
                                  graph,
                                  fun = "climatology",
                                  arg.list = NULL) {
    orig.node <- graph$parentnodename
    graph <- graph$graph
    if (class(graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    # Climatology -----------------------------------------------
    clim.nodename <- paste("Climatology", randomName(), sep = ".")
    if (is.null(arg.list)) arg.list <- list(clim.fun = list(FUN = "mean", na.rm = TRUE), by.member = TRUE)
    cellme <- paste(deparse(arg.list$clim.fun$FUN), collapse = "")
    cellme <- gsub("\"","'", cellme)
    graph <- add_vertices(graph,
                          nv = 1,
                          name = clim.nodename,
                          label = "Climatology",
                          className = "ds:Climatology",
                          description = "Climatology Class",
                          attr = list("ds:hasCellMethod" = cellme))
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, orig.node),
                         getNodeIndexbyName(graph, clim.nodename)),
                       label = "ds:hadClimatology")
    # Package/Command/Argument metadata ---------------------------------------
    if ("grid" %in% names(arg.list)) arg.list <- arg.list[-grep("grid", names(arg.list))]
    graph <- metaclip.graph.Command(graph, package, version, fun, arg.list,
                                    origin.node.name = clim.nodename)
    return(list("graph" = graph, "parentnodename" = clim.nodename)) 
}
