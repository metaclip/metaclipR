##     metaclipR.Binding Construct a METACLIP representation of the ds:Binding class
##
##     Copyright (C) 2020 SantanderMetGroup (http://www.meteo.unican.es)
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

#' @title Construct a METACLIP representation of the ds:Binding class
#' @description Build a directed metadata graph describing a binding transformation from two or more
#' ds:Steps along a chosen dimension
#' @param graph.list A \code{metaclipR} object data list, each element being the subelements to be binded
#' @param dim.along Dimension along which data are binded. Accepted values are \code{"time"}, \code{"lat"} and \code{"lon"}. See Details.
#' @param dc.description Default to \code{NULL} and unused. Otherwise, this is a character string that will be appendend as a
#'  "dc:description" annotation to the ds:Ensemble-class node.
#' @details The special case in which different subsets of the same dataset are binded along the member dimension is sepparately treated by the Ensemble class,
#' described via \code{\link{metaclipR.Ensemble}}.
#' 
#' This function takes as reference the semantics defined in the Datasource and Transformation ontology (\url{www.metaclip.org/datasource/datasource.owl})
#' defined in the Metaclip Framework (\url{http://www.metaclip.org}).
#' @family transformation
#' @export
#' @importFrom igraph add_edges 
#' @author J. Bedia

metaclipR.Binding <- function(graph.list,
                              dim.along,
                              dc.description = NULL) {
    if (length(graph.list) < 2) {
        stop("The input must be a list of at least two metaclipR graphs", call. = FALSE)
    }
    for (i in 1:length(graph.list)) {
        if (class(graph.list[[i]]$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")    
    }
    dim.along <- match.arg(dim.along, choices = c("time", "lat", "lon"))
    if (dim.along == "time") {
        dim.class <- "ds:ValidationTime"
        dim.label <- "Time"
    } else if (dim.along == "lat") {
        dim.class <- "ds:Latitude"
        dim.label <- "Latitude"
    } else if (dim.along == "lon") {
        dim.class <- "ds:Longitude"
        dim.label <- "Longitude"
    }
    # Ensemble node
    graph <- graph.list[[1]]$graph
    # graph <- make_empty_graph()
    nodename <- paste0("Binding.", randomName())
    if (is.null(dc.description)) {
        graph <- my_add_vertices(graph,
                                 name = nodename,
                                 label = "Dataset Binding",
                                 className = "ds:Binding")    
    } else {
        graph <- my_add_vertices(graph,
                                 name = nodename,
                                 label = "Dataset Binding",
                                 className = "ds:Binding", attr = list("dc:description" = dc.description))    
    }
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, graph.list[[1]]$parentnodename),
                         getNodeIndexbyName(graph, nodename)),
                       label = "ds:hadBinding")    
    for (i in 2:length(graph.list)) {
        graph <- my_union_graph(graph, graph.list[[i]]$graph)
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, graph.list[[i]]$parentnodename),
                             getNodeIndexbyName(graph, nodename)),
                           label = "ds:hadBinding")
    }
    ## Combination method (if specified)
    dim.nodename <- setNodeName(dim.along, node.class = "Dimension", vocabulary = "datasource")
    graph <- my_add_vertices(graph = graph,
                             name = dim.nodename,
                             label = dim.label,
                             className = dim.class)
    graph <- add_edges(graph, c(getNodeIndexbyName(graph, nodename),
                                getNodeIndexbyName(graph, dim.nodename)),
                       label = "ds:alongDimension")
    return(list("graph" = graph, "parentnodename" = nodename))
}
