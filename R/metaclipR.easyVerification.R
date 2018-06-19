##     metaclipR.easyVerification Construct a directed graph for verification
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

#' @title Directed metadata graph construction for outputs
#' @description Build a directed metadata graph from veriApply calls, and other verification routines
#' @param version A character string of the package version (e.g. as returned by \code{\link[utils]{packageVersion}})
#' @param package Verification package. Default to \code{"easyVerification"}
#' @param fun Verification function. Default to \code{"veriApply"}.
#' @template template_arglistParam
#' @template template_arglist
#' @param ForecastGraph metaclipR output containing the forecast (hindcast) definition
#' @param ReferenceGraph metaclipR output  containing the reference (observations) definition
#' @param ReferenceForecastGraph Optional. metaclipR output containing the reference forecast definition

#' @details This function takes as reference the semantics defined in the Seasonal Forecast Verification ontology
#' defined in the Metaclip Framework (\url{http://www.metaclip.org}).
#' @export
#' @importFrom igraph make_empty_graph add_vertices add_edges 
#' @author D. San Martín, J. Bedia
#' @family validation

metaclipR.easyVerification <- function(package = "easyVerification",
                                       version = as.character(packageVersion(package)),
                                       fun,
                                       arg.list, 
                                       ForecastGraph,
                                       ReferenceGraph,
                                       ReferenceForecastGraph = NULL) {
    .Deprecated(new = "metaclipR.Validation")
    if (is.null(arg.list)) stop("Argument list required to encode 'easyVerification' metadata", call. = FALSE)
    if (fun == "veriApply") {
        arg.names <- c("verifun", "tdim", "ensdim", "na.rm", "fcst.ref", "prob", "threshold", "strategy")
        if (!all(arg.names %in% names(arg.list))) stop("Incomplete 'arg.list': required argument/value pairs are missing",
                                                       call. = FALSE)
        vfun <- arg.list$verifun
    } else {
        vfun <- fun
    }
    # First, an empty graph is created
    graph <- make_empty_graph(0)
    measure <- getVerificationLabel(vfun)
    origin.node.name <- paste0("Verification.", randomName())
    graph <- add_vertices(graph,
                          nv = 1,
                          name = origin.node.name,
                          label = measure,
                          className = "veri:ForecastVerification")
    # ForecastRepresentation Class
    ForecastRepresentation <- getForecastRepresentation(vfun)
    hasProbabilityThreshold <- getHasProbabilityThreshold(ForecastRepresentation, arg.list)
    hasAbsoluteThreshold <- getHasAbsoluteThreshold(ForecastRepresentation, arg.list)
    forecastRepresentation.node.name <- paste("ForecastRepresentation", randomName(), sep = ".")
    graph <- add_vertices(graph, 
                          nv = 1,
                          name = forecastRepresentation.node.name, 
                          label = ForecastRepresentation,
                          className = ForecastRepresentation,
                          description = "Numerical representation of the forecast",
                          attr = c(hasProbabilityThreshold, hasAbsoluteThreshold))
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, origin.node.name),
                         getNodeIndexbyName(graph, forecastRepresentation.node.name)),
                       label = "veri:withForecastRepresentation")
    # QualityAspect Class
    QualityAspect <- getQualityAspect(vfun)
    qualityAspect.node.name <- paste("QualityAspect", randomName(), sep = ".")
    graph <- add_vertices(graph, 1, name = qualityAspect.node.name, 
                          className = QualityAspect,
                          label = QualityAspect,
                          description = "Quality aspect addressed by the verification measure")
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, origin.node.name),
                         getNodeIndexbyName(graph, qualityAspect.node.name)),
                       label = "veri:withQualityAspect")
    # Linking the verification with the hindcast
    graph <- my_union_graph(graph, ForecastGraph$graph)
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, ForecastGraph$parentnodename),
                         getNodeIndexbyName(graph, origin.node.name)),
                       label = "veri:hadValidation")
    # Linking the verification with the observations
    graph <- my_union_graph(graph, ReferenceGraph$graph)
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, origin.node.name),
                         getNodeIndexbyName(graph, ReferenceGraph$parentnodename)),
                       label = "veri:withReference")
    # Linking the verification with the referfence forecast (skill scores only)
    if (!is.null(ReferenceForecastGraph)) {
        graph <- my_union_graph(graph, ReferenceForecastGraph$graph)
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, origin.node.name),
                             getNodeIndexbyName(graph, ReferenceForecastGraph$parentnodename)),
                           label = "veri:withReferenceForecast")
    }
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

