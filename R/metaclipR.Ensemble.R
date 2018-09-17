##     metaclipR.Ensemble Construct a METACLIP representation of the ds:Ensemble class
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

#' @title Construct a METACLIP representation of the ds:Ensemble class
#' @description Build a directed metadata graph describing an Ensemble transformation from two or more
#' ds:Steps
#' @param package package name. Default to \code{"transformeR"}
#' @param version Package version string.
#' @param fun function name. Default to \code{"bindGrid.member"})
#' @param graph.list A \code{metaclipR} object data list, each element being the
#' graph defining each ensemble member
#' @param output Optional. The output R object name, as character string
#' @param combination.method Optional. Character string refering to the combination method used to construct the ensemble.
#' This is represented by the ds class "CombinationMethod", for which several individual instances exist (this
#' can be indicated here). Type \code{knownClassIndividuals("CombinationMethod")} for further details.
#' @param disable.command Better not to touch. For internal usage only (used to re-use most of the code in other
#'  functions, but skipping command tracking)
#' @template template_arglistParam
#' @template template_arglist
#' @details This function takes as reference the semantics defined in the Data Source and Transformation ontology
#' defined in the Metaclip Framework (\url{http://www.metaclip.org}).
#' @family transformation
#' @export
#' @importFrom igraph add_vertices add_edges 
#' @author D. San Martín, J. Bedia

metaclipR.Ensemble <- function(package = "transformeR",
                               version = as.character(packageVersion(package)),
                               output = NULL,
                               fun = "bindGrid.member",
                               arg.list = NULL,
                               combination.method = NULL,
                               graph.list,
                               disable.command = FALSE) {
    if (length(graph.list) < 2) {
        stop("The input must be a list of at least two metaclipR graphs", call. = FALSE)
    }
    for (i in 1:length(graph.list)) {
        if (class(graph.list[[i]]$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")    
    }
    stopifnot(is.logical(disable.command))
    # Ensemble node
    graph <- graph.list[[1]]$graph
    # graph <- make_empty_graph()
    nodename <- paste0("Ensemble.", randomName()) 
    graph <- my_add_vertices(graph,
                             name = nodename,
                             label = "Multi-model Ensemble",
                             className = "ds:Ensemble")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, graph.list[[1]]$parentnodename),
                         getNodeIndexbyName(graph, nodename)),
                       label = "ds:wasEnsembleMember")
    for (i in 2:length(graph.list)) {
        graph <- my_union_graph(graph, graph.list[[i]]$graph)
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, graph.list[[i]]$parentnodename),
                             getNodeIndexbyName(graph, nodename)),
                           label = "ds:wasEnsembleMember")
    }
    ## Combination method (if specified)
    if (!is.null(combination.method)) {
        if (!is.character(combination.method)) stop("Invalid \'combination.method\' argument value", call. = FALSE)
        cm.nodename <- setNodeName(combination.method, node.class = "CombinationMethod", vocabulary = "datasource")
        graph <- my_add_vertices(graph = graph,
                                 name = cm.nodename,
                                 label = "Combination",
                                 className = "ds:CombinationMethod")
        graph <- add_edges(graph, c(getNodeIndexbyName(graph, nodename),
                                    getNodeIndexbyName(graph, cm.nodename)),
                           label = "ds:hadCombinationMethod")
    }
    # Package/Command/Argument metadata ---------------------------------------
    if (!disable.command) {
        graph <- metaclip.graph.Command(graph, package, version, fun, arg.list,
                                        origin.node.name = nodename)
    }
    return(list("graph" = graph, "parentnodename" = nodename))
}
