##     metaclipR.BiasCorrection Construct a directed graph for bias correction
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

#' @title Directed metadata graph construction for bias correction steps
#' @description Build a directed metadata graph from bias correction routines
#' @param version A character string of the package version (e.g. as returned by \code{\link[utils]{packageVersion}})
#' @param package bias correction package. Default to \code{"downscaleR"}
#' @param fun Validation function. Default to \code{"biasCorrection"}.
#' @template template_arglistParam
#' @template template_arglist
#' @param hasProbCharacter Default to NULL (unspecified). It referes to the probabilistic nature of the method. 
#' Two possible disjoint values: \code{"deterministic"} or \code{"stochastic"}
#' @param isMultisite Default to NULL (unspecified), otherwise logical. Is the bias correction method envisaged to 
#' deal with multisite predictions? See References.
#' @param isMultivariable Default to NULL (unspecified). isMultisite Default to NULL (unspecified), otherwise logical. Is the bias correction method envisaged to 
#' deal with multisite predictions? See References.
#' @param comment An optional character string with a comment on the method (encoded as rdfs:comment data property
#'  associated to the calibration method)
#' @param BC.method Character string indicating the name of the bias correction method.
#'  For a list of known (instantiable) BiasCorrection methods see \code{knownClassIndividuals("BiasCorrection", "calibration")}
#' @param BC.class Character string indicating the specific subclass of the Calibration superclass being asserted.
#' Accepted values are defined in the Calibration ontology (see Details). By default, the superclass \code{"BiasCorrection"}
#' is assigned, but other more specific subclasses should be indicated when relevant.
#' @param isDefinedBy An optional character string indicating where or by who the method has been defined 
#' (e.g. paper publication etc.). This is encoded as rdfs:isDefinedBy data property associated to the calibration method).
#' @param graph metaclipR output containing the data top be bias-corrected.
#' @param TrainingGraph metaclipR output containing the training data (e.g. 20C3M/historical scenario in
#'  climate change applications etc.)
#' @param ReferenceGraph metaclipR output containing the reference predictand (typically observations) 
#' @param disable.command Better not to touch. For internal usage only (used to re-use most of the code in
#'  other functions, but skipping command tracking)
#' @details This function takes as reference the semantics defined in the Calibration ontology defined in the 
#' Metaclip Framework (\url{http://www.metaclip.org/}). These in turn are partially based on the VALUE Framewrok (Gutiérrez et al. 2018)
#' @references 
#' Gutiérrez et al, 2018. An intercomparison of a large ensemble of statistical downscaling methods over Europe: 
#' Results from the VALUE perfect predictor cross-validation experiment. International Journal of Climatology. 
#' https://doi.org/10.1002/joc.5462

#' @export
#' @importFrom igraph make_empty_graph add_vertices add_edges 
#' @author D. San Martín, J. Bedia
#' @family calibration

metaclipR.BiasCorrection <- function(package = "downscaleR",
                                     version = "3.0.0",
                                     fun = "biasCorrection",
                                     arg.list,
                                     graph,
                                     TrainingGraph,
                                     ReferenceGraph,
                                     BC.method,
                                     BC.class = "BiasCorrection",
                                     isDefinedBy = NULL,
                                     comment = NULL,
                                     hasProbCharacter = NULL,
                                     isMultivariable = NULL,
                                     isMultisite = NULL,
                                     disable.command = FALSE) {
    if (class(graph$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    if (class(TrainingGraph$graph) != "igraph") stop("Invalid input TrainingGraph (not an 'igraph-class' object)")
    if (class(ReferenceGraph$graph) != "igraph") stop("Invalid input ReferenceGraph (not an 'igraph-class' object)")
    stopifnot(is.logical(disable.command))
    pkgVersionCheck(package, version)
    bc.class <- getIndividualClass(BC.method, vocabulary = "calibration")
    if (!is.null(bc.class)) {
        BC.class <- bc.class
    } else {
        BC.class <- match.arg(BC.class, choices = c("BiasCorrection",
                                                    "NonParametricBiasCorrection",
                                                    "LinearScaling",
                                                    "NPQuantileMapping",
                                                    "PQuantileMapping",
                                                    "ParametricBiasCorrection",
                                                    "VarianceInflation",
                                                    "ProbabilityScaling",
                                                    "EnsembleRecalibration"))
    }
    if (!is.null(hasProbCharacter)) {
        hasProbCharacter <- match.arg(hasProbCharacter, choices = c("deterministic", "stochastic"))
    }
    pnode <- graph$parentnodename
    graph <- graph$graph
    # Adding the Calibration node
    cal.node <- paste0("Calibration.", randomName())
    graph <- my_add_vertices(graph,
                             name = cal.node,
                             label = "Calibration",
                             className = "cal:Calibration")
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, pnode),
                         getNodeIndexbyName(graph, cal.node)),
                       label = "cal:hadCalibration")
    # Adding the CalibrationMethod node
    isKnownMethod <- ifelse(BC.method %in% suppressMessages(knownClassIndividuals("CalibrationMethod", vocabulary = "calibration")), TRUE, FALSE)
    # if (isKnownDataset) Dataset.subclass <- getIndividualClass(Dataset.name)
    # method.nodename <- setNodeName(node.name = BC.method, node.class = BC.class, vocabulary = "calibration")
    method.nodename <- if(isKnownMethod) {
        paste0("cal:", BC.method)
        BC.class <- getIndividualClass("EQM", vocabulary = "calibration")
    } else {
        setNodeName(node.name = BC.method, node.class = BC.class, vocabulary = "calibration")
    }
    if (is.null(bc.class)) {
        attrl <- list("cal:isMultiVariable" = isMultivariable,
                      "cal:isMultiSite" = isMultisite,
                      "cal:hasProbCharacter" = hasProbCharacter,
                      "rdfs:isDefinedBy" = isDefinedBy,
                      "rdfs:comment" = comment)
    } else {
        attrl <- NULL    
    }
    graph <- my_add_vertices(graph,
                             name = method.nodename,
                             label = BC.method,
                             className = BC.class,
                             attr = attrl)
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, cal.node),
                         getNodeIndexbyName(graph, method.nodename)),
                       label = "cal:withCalibrationMethod")
    # Adding the training Data
    graph <- my_union_graph(graph, TrainingGraph$graph)
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, cal.node),
                         getNodeIndexbyName(graph, TrainingGraph$parentnodename)),
                       label = "cal:withTrainingData")
    # Adding the predictand Data
    graph <- my_union_graph(graph, ReferenceGraph$graph)
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, cal.node),
                         getNodeIndexbyName(graph, ReferenceGraph$parentnodename)),
                       label = "cal:withReferenceData")
    # Function call 
    if (!disable.command) {
        if ("y" %in% names(arg.list)) arg.list <- arg.list[-grep("y", names(arg.list), fixed = TRUE)]
        if ("x" %in% names(arg.list)) arg.list <- arg.list[-grep("x", names(arg.list), fixed = TRUE)]
        if ("newdata" %in% names(arg.list)) arg.list <- arg.list[-grep("newdata", names(arg.list), fixed = TRUE)]
        graph <- metaclip.graph.Command(graph,
                                        package = package,
                                        version = version,
                                        fun = fun,
                                        arg.list = arg.list,
                                        origin.node.name = cal.node)
    }
    return(list("graph" = graph, "parentnodename" = cal.node))
}

