##     metaclipR.loadeR Construct a directed graph for loadeR output provenance description
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

#' @title Directed metadata graph construction for loadeR outputs 
#' @description Build a directed metadata graph from loadeR outputs, that are subsets (and possibly 
#' aggregations and/or climate indices) of Datasets.
#' @param package loading climate4R package. Default to \code{"loadeR"} (but see argument \code{fun})
#' @param version package version 
#' @param graph A previously initialized metaclipR output list ($graph + $parentnodename). 
#' @param fun function name. Default to \code{"loadGridData"}, but other loading functions are accepted, namely 
#' loadStationData from package \pkg{loadeR}, \code{loadECOMS}, from package \pkg{loadeR.ECOMS} or \code{loadValueStations}
#'  and \code{loadValuePredictions} from package \pkg{R.VALUE}.
#' @template template_arglistParam
#' @template template_arglist
#' @param index.name Xharacter string (default to "none"). In case of using loadGridData, it exists the possibility of
#' applying filters to monthly aggregations, leading to specific climate indices with particular names.
#'  Some of these are instances of the ds:ClimateIndex class (e.g. "SU", for the ETCCDI index 'summer days'). 
#'  An overview of already existing indices see \code{knownClassIndividuals("ClimateIndex")}.
#' @param output A climate4R grid, resulting from the application of the specified \code{fun} function. 
#' The name of the grid is passed as a character string
#' @details This function takes as reference the semantics defined in the Data Source and Transformation ontology
#' defined in the Metaclip Framework (\url{http://www.metaclip.org/}).
#' 
#' @return A named list with the updated graph in element \code{graph} and the terminal node in \code{parentnodename},
#' needed for linking subsequent operations.
#' 
#' @export
#' @importFrom igraph add_vertices add_edges 
#' @importFrom transformeR getShape getSeason getRefDates getGrid
#' @importFrom utils tail
#' @author D. San Martín, J. Bedia
#' @family subsetting

metaclipR.loadeR <- function(package = "loadeR",
                             version = as.character(packageVersion(package)),
                             graph,
                             fun = NULL,
                             arg.list = NULL,
                             index.name = "none",
                             output = NULL) {
    
    orig.nodes.command <- c()
    # Data subsetting
    graph <- metaclipR.DatasetSubset(graph = graph, output = output, disable.command = TRUE)
    orig.nodes.command <- c(orig.nodes.command, graph$parentnodename)
    graph <- graph$graph
    # Encode Index calculation
    if ("condition" %in% names(arg.list) && "threshold" %in% names(arg.list) && !is.null(arg.list$threshold) && !is.null(arg.list$condition)) {
        # Climate index node
        nodename <- paste("ClimateIndex", randomName(), sep = ".")
        # Instance definition
        instance <- ifelse(index.name %in% knownClassIndividuals("ClimateIndex"), index.name, "ClimateIndex")
        graph <- add_vertices(graph,
                              nv = 1,
                              name = nodename,
                              label = instance,
                              className = paste0("ds:", instance))
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, tail(orig.nodes.command, 1)),
                             getNodeIndexbyName(graph, nodename)),
                           label = "ds:hadClimateIndexCalculation")    
        orig.nodes.command <- c(orig.nodes.command, nodename)
    } # else if { -- encode aggregations of udg datasets, whose original temporal resolution is known  }
    # ARGUMENT - COMMAND METADATA ----------------------------------------------
    graph <- metaclip.graph.Command(graph, package, version, fun, arg.list,
                                    origin.node.name = orig.nodes.command)
    return(list("graph" = graph,
                "parentnodename" = tail(orig.nodes.command, 1)))
}
    
    
