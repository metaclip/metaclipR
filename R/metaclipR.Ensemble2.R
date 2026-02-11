##     metaclipR.Ensemble2 Construct a METACLIP representation of the ds:Ensemble class
##
##     Copyright (C) 2026 Santander Meteorology Group (SMG), University of Cantabria, Spain
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

#' @title Construct a METACLIP representation of the ds:Ensemble class and ds:GrandEnsemble subclass
#' @description Build a directed metadata graph describing an Ensemble transformation from two or more
#' ds:Steps
#' @param graph.list A \code{metaclipR} object data list, each element being the
#' graph defining each ensemble member
#' @param grand.ensemble Logical. If \code{TRUE}, the resulting ds:Ensemble will be annotated as its sub-class ds:GrandEnsemble.
#' @param named.individual Character string. The resulting ds:Ensemble (or its subclass ds:GrandEnsemble if \code{grand.ensemble} is set to \code{TRUE})
#'  will be annotated as a named individual (i.e. with a "ds:name" annotation). Default to \code{NULL} and not used.
#' @param dc.description Default to \code{NULL} and unused. Otherwise, this is a character string that will be appendend as a
#'  "dc:description" annotation to the ds:Ensemble-class node.
#' @details This function takes as reference the semantics defined in the Data Source and Transformation ontology
#' defined in the Metaclip Framework (\url{http://www.metaclip.org}).
#' @family transformation
#' @note This function is a variant of \code{\link{metaclipR.Ensemble}} that allows to create both ds:Ensemble 
#' and ds:GrandEnsemble instances, and to annotate them as named individuals.
#' The original function is still available for backward compatibility,
#' but it is recommended to use this new one instead.
#' @export
#' @importFrom igraph add_vertices add_edges 
#' @author J. Bedia

metaclipR.Ensemble2 <- function(graph.list,
                                grand.ensemble = FALSE,
                                named.individual = NULL,
                                dc.description = NULL) {
    if (length(graph.list) < 2) {
        stop("The input must be a list of at least two metaclipR graphs", call. = FALSE)
    }
    for (i in 1:length(graph.list)) {
        if (class(graph.list[[i]]$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")    
    }
    stopifnot(is.logical(grand.ensemble))

    # Ensemble node characteristics
    if (isTRUE(grand.ensemble)) {
        className <- "ds:GrandEnsemble"
        nodename <- paste0("GrandEnsemble.", randomName())
        label <- "Grand Ensemble"
    } else {
        className <- "ds:Ensemble"
        nodename <- paste0("Ensemble.", randomName())
        label <- "Ensemble"
    }
    
    ## Override nodename if named.individual is provided
    if (!is.null(named.individual)) {
        nodename <- named.individual
    }

    graph <- graph.list[[1]]$graph

    ## dc:description annotation (optional)  
    if (is.null(dc.description)) {
        graph <- my_add_vertices(graph,
                                 name = nodename,
                                 label = label,
                                 className = className)    
    } else {
        graph <- my_add_vertices(graph,
                                 name = nodename,
                                 label = label,
                                 className = className,
                                 attr = list("dc:description" = dc.description))    
    }
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, graph.list[[1]]$parentnodename),
                         getNodeIndexbyName(graph, nodename)),
                       label = "ds:isMemberOf")

    for (i in 2:length(graph.list)) {
        graph <- my_union_graph(graph, graph.list[[i]]$graph)
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, graph.list[[i]]$parentnodename),
                             getNodeIndexbyName(graph, nodename)),
                           label = "ds:isMemberOf")
    }
    return(list("graph" = graph, "parentnodename" = nodename))
}
