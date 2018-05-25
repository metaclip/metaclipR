##     metaclipR.Validation Construct a directed graph for Validation steps
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

#' @title Directed metadata graph construction for validation steps
#' @description Build a directed metadata graph from validation routines
#' @param version A character string of the package version (e.g. as returned by \code{\link[utils]{packageVersion}})
#' @param package Validation package. 
#' @param fun Validation function.
#' @param arg.list argument list passed by \code{fun}.
#' @param QualityAspect Class name. Quality Aspect addressed by the Validation. Possible values are 
#' \code{"Bias"}, \code{"Accuracy"}, \code{"Association"}, \code{"Reliability"}, \code{"Discrimination"} and \code{"Resolution"}.
#' @param PredictionGraph metaclipR output containing the Predictions/Projections to validate
#' @param ReferenceGraph metaclipR output containing the reference (observations) 
#' @details This function takes as reference the semantics defined in the Verification ontology defined in the 
#' Metaclip Framework (\url{http://www.metaclip.org}).
#' @export
#' @importFrom igraph make_empty_graph add_vertices add_edges 
#' @author D. San Martín, J. Bedia
#' @family validation

metaclipR.Validation <- function(package,
                                 version,
                                 fun,
                                 QualityAspect,
                                 arg.list, 
                                 PredictionGraph,
                                 ReferenceGraph) {
    if (class(PredictionGraph$graph) != "igraph") stop("Invalid input PredictionGraph (not an 'igraph-class' object)")   
    if (class(ReferenceGraph$graph) != "igraph") stop("Invalid input ReferenceGraph (not an 'igraph-class' object)")   
    pgraph <- PredictionGraph$graph
    refgraph <- ReferenceGraph$graph
    pnode <- PredictionGraph$parentnodename
    refnode <- ReferenceGraph$parentnodename
    QualityAspect <- match.arg(QualityAspect, choices = c("Accuracy",
                                                          "Association",
                                                          "Bias",
                                                          "Discrimination",
                                                          "Reliability",
                                                          "Resolution"))
    origin.node.name <- paste0("Validation.", randomName())
    pgraph <- add_vertices(pgraph,
                           nv = 1,
                           name = origin.node.name,
                           label = QualityAspect,
                           className = "veri:Validation",
                           description = "Metadata to encode validation")
    pgraph <- add_edges(pgraph, 
                        c(getNodeIndexbyName(pgraph, pnode),
                          getNodeIndexbyName(pgraph, origin.node.name)),
                        label = "veri:hadValidation")
    # Linking the verification with the reference
    graph <- my_union_graph(pgraph, refgraph)
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, origin.node.name),
                         getNodeIndexbyName(graph, refnode)),
                       label = "veri:withReference")
    # Linking with QualityAspect
    qnode.name <- paste0("QualityAspect.", randomName())
    graph <- add_vertices(graph,
                          nv = 1,
                          name = qnode.name,
                          label = QualityAspect,
                          className = paste0("veri:", QualityAspect))
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, origin.node.name),
                         getNodeIndexbyName(graph, qnode.name)),
                       label = "veri:withQualityAspect")
    # Function call 
    if ("grid" %in% names(arg.list)) arg.list <- arg.list[-grep("grid", names(arg.list))]
    graph <- metaclip.graph.Command(graph,
                                    package = package,
                                    version = version,
                                    fun = fun,
                                    arg.list = arg.list,
                                    origin.node.name = origin.node.name)
    return(list("graph" = graph, "parentnodename" = origin.node.name))
}

