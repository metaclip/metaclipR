##     metaclipR.etccdi Construct a directed graph for encoding ETCDDI climate index transformations
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

#' @title Directed metadata graph construction for ETCDDI climate index transformations
#' @description Build a directed metadata graph describing a ETCDDI climate index transformation on a 
#' climate4R grid 
#' @param package package
#' @param version version
#' @param index.code Character string, indicating the specific code of the index according to the ETCCDI definitions (see Details)
#' @param fun function name. Unused (set to \code{"climdexGrid"})
#' @param output Optional. The output R object name, as character string
#' @template template_arglistParam
#' @template template_arglist
#' @param graph Output from previous metaclipR function. A list with an i-graph class object plus the name of the parent node 
#' from which the climate index step hangs. \code{climdexGrid} from package \pkg{climate4R.climdex} is indicated in argument \code{fun}.
#' @details This function takes as reference the semantics defined in the Data Source and Transformation ontology
#' defined in the Metaclip Framework (\url{http://www.metaclip.org}).
#' 
#' The index codes are those presented in the TCDDI web page, giving the definition of the 27 core indices: http://etccdi.pacificclimate.org/list_27_indices.shtml
#' 
#' Alternatively, the function \code{climdexShow} from package \pkg{climate4R.climdex} will display on screen a
#'  full list of ETCCDI Core indices and their codes. 
#' 
#' @family transformation
#' @export
#' @importFrom igraph make_empty_graph add_vertices add_edges 
#' @author D. San Martín, J. Bedia
#' @examples \dontrun{
#' require(climate4R.climdex)
#' require(igraph)
#' data("tasmax.eobs")
#' a <- metaclipR.DatasetSubset(output = "tasmax.eobs",
#'                              fun = "subsetGrid",
#'                              arg.list = list(lonLim = c(-10,4.5),
#'                              latLim = c(35,44),
#'                              season = 1:12,
#'                              years = 1991:2010))
#'
#' tx10p <- climdexGrid(index.code = "TX10p",
#'                      tx = tasmax.eobs,
#'                      index.arg.list = list(freq = "annual"))
#' 
#' arg.list <- list(index.code = "TX10p",
#'                  index.arg.list = list(freq = "annual"))
#' 
#' require(visualizeR)
#' spatialPlot(climatology(tx10p),
#'             backdrop.theme = "countries",
#'             main = "Mean percentage of days when TX < 10 degC (1991-2010)")
#' metadata <- metaclipR.etccdi(graph = a, index.code = "TX10p", arg.list = arg.list)            
#' plot(metadata$graph)
#' 
#' # Alternatively, since metaclipR 1.1.0 the literal command call can be used:
#' cc <- "tx10p <- climdexGrid(index.code = \"TX10p\", tx = tasmax.eobs, index.arg.list = list(freq = \"annual\"))"
#' metadata2 <- metaclipR.etccdi(graph = a, index.code = "TX10p", arg.list = cc)            
#' plot(metadata2$graph)
#' }


metaclipR.etccdi <- function(graph,
                             package = "climate4R.climdex",
                             version = as.character(packageVersion(package)),
                             output = NULL,
                             index.code,
                             fun = "climdexGrid",
                             arg.list = NULL) {
    if (class(graph$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    if (is.null(index.code)) {
        stop("The 'index.code' argument is missing in the argument list, with no default")
    }
    index.code <- match.arg(index.code,
                            choices = c("FD", "SU", "ID", "TR", "GSL", "TXx", "TNx", "TXn",
                                        "TNn", "TN10p", "TX10p", "TN90p", "TX90p", "WSDI", "CSDI",
                                        "DTR", "Rx1day", "Rx5day", "SDII", "R10mm", "R20mm",
                                        "Rnnmm", "CDD", "CWD", "R95pTOT", "R99pTOT", "PRCPTOT"))
    if (is.list(arg.list) && ("index.code" %in% names(arg.list))) {
        if (arg.list$index.code != index.code) stop("Conflicting \'arg.list$index.code\' and \'index.code\' argument values")
    }
    orig.node <- graph$parentnodename
    graph <- graph$graph
    cicalc.node <- paste("ClimateIndexCalculation", randomName(), sep = ".")
    # ClimateIndex calculation node
    graph <- my_add_vertices(graph,
                             nv = 1,
                             name = cicalc.node,
                             label = "ClimateIndexCalculation",
                             className = "ds:ClimateIndexCalculation")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, orig.node),
                         getNodeIndexbyName(graph, cicalc.node)),
                       label = "ds:hadClimateIndexCalculation")
    # Climate index node
    isKnownIndex <- ifelse(index.code %in% suppressMessages(knownClassIndividuals("ETCCDI")), TRUE, FALSE)
    if (isKnownIndex) {
        nodename <- paste0("ds:", index.code)
        cn <- "ds:ETCCDI"
    } else {
        nodename <- paste0("CimateIndex.", randomName())
        cn <- "ds:ClimateIndex"
    }
    graph <- my_add_vertices(graph,
                             nv = 1,
                             name = nodename,
                             label = paste("ClimateIndex", index.code, sep = "."),
                             className = cn)
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, cicalc.node),
                         getNodeIndexbyName(graph, nodename)),
                       label = "ds:withClimateIndex")
    # TemporalResolution ---------------------
    # hasTimeStep
    if (!is.null(output)) {
        output <- get(output)
        output$Data <- NULL
        time.step <- getHasTimeStep(output)
        output <- NULL
        cell.method <- index.code
        timeres.nodename <- paste("TemporalResolution", randomName(), sep = ".")
        graph <- add_vertices(graph,
                              nv = 1,
                              name = timeres.nodename,
                              label = "TemporalResolution",
                              className = "ds:TemporalResolution",
                              attr = list("ds:hasTimeStep" = time.step,
                                          "ds:hasCellMethod" = cell.method))
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, cicalc.node),
                             getNodeIndexbyName(graph, timeres.nodename)),
                           label = "ds:hasTemporalResolution")
    }
    # Package/Command/Argument metadata ---------------------------------------
    if ("tn" %in% names(arg.list)) arg.list <- arg.list[-grep("tn", names(arg.list))]
    if ("tx" %in% names(arg.list)) arg.list <- arg.list[-grep("tx", names(arg.list))]
    if ("pr" %in% names(arg.list)) arg.list <- arg.list[-grep("pr", names(arg.list))]
    graph <- metaclip.graph.Command(graph, package, version, fun, arg.list,
                                    origin.node.name = cicalc.node)
    return(list("graph" = graph, "parentnodename" = cicalc.node))
}



#' @title METACLIP description of climate index calculation steps
#' @description METACLIP description of climate index calculation steps
#' @param graph Output from previous metaclipR function. A list with an i-graph class object plus the name of the parent node 
#' from which the climate index step hangs. \code{climdexGrid} from package \pkg{climate4R.climdex} is indicated in argument \code{fun}.
#' @param package package
#' @param version version
#' @param index.code Character string, indicating the specific code of the index according to the ETCCDI definitions (see Details)
#' @param fun function name. Unused (set to \code{"climdexGrid"})
#' @param output Optional. The output climate4R object name, as character string
#' @template template_arglistParam
#' @template template_arglist
#' @param new.time.res Character string. Recommended an ISO8601 representation of temporal resolution, 
#' as a "duration". Typical values are "P1H" (hourly), "P3H" (3-hourly), "P1D" (daily), "P1M" (monthly) etc.
#' If omitted, the last temporal resolution given is assumed to be preserved, and this is not updated.
#' @param time.step Optional. Time step data property
#' @param cell.method Optional. Cell method. Default to the \code{index.code} value. 
#' @references For reference on ISO8601 definition \url{https://en.wikipedia.org/wiki/ISO_8601#Durations}
#' @export

metaclipR.ClimateIndex <- function(graph,
                                   package = "climate4R.climdex",
                                   version = as.character(packageVersion(package)),
                                   output = NULL,
                                   index.code,
                                   fun = "climdexGrid",
                                   arg.list = NULL,
                                   new.time.res = NULL,
                                   time.step = NULL,
                                   cell.method = NULL) {
    if (class(graph$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    if (is.null(index.code)) {
        stop("The 'index.code' argument is missing in the argument list, with no default")
    }
    orig.node <- graph$parentnodename
    graph <- graph$graph
    cicalc.node <- paste("ClimateIndexCalculation", randomName(), sep = ".")
    # ClimateIndex calculation node
    graph <- my_add_vertices(graph,
                             name = cicalc.node,
                             label = "ClimateIndexCalculation",
                             className = "ds:ClimateIndexCalculation")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, orig.node),
                         getNodeIndexbyName(graph, cicalc.node)),
                       label = "ds:hadClimateIndexCalculation")
    # Climate index node
    nodename <- setNodeName(node.name = index.code, node.class = "ClimateIndex")
    aux <- suppressWarnings(suppressMessages(getIndividualClass(index.code)))
    cn <- ifelse(is.null(aux),"ds:ClimateIndex", paste0("ds:", index.code))
    graph <- my_add_vertices(graph,
                             name = nodename,
                             label = index.code,
                             className = cn)
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, cicalc.node),
                         getNodeIndexbyName(graph, nodename)),
                       label = "ds:withClimateIndex")
    # TemporalResolution ---------------------
    # hasTimeStep
    if (!is.null(output)) {
        output <- get(output)
        output$Data <- NULL
        time.step <- getHasTimeStep(output)
        output <- NULL
        cell.method <- index.code
        timeres.nodename <- paste("TemporalResolution", randomName(), sep = ".")
        graph <- add_vertices(graph,
                              nv = 1,
                              name = timeres.nodename,
                              label = "TemporalResolution",
                              className = "ds:TemporalResolution",
                              attr = list("ds:hasTimeStep" = time.step,
                                          "ds:hasCellMethod" = cell.method))
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, cicalc.node),
                             getNodeIndexbyName(graph, timeres.nodename)),
                           label = "ds:hasTemporalResolution")
    } else {
        if (!is.null(new.time.res)) {
            if (is.null(cell.method)) cell.method <- index.code
            if (is.null(time.step)) time.step <- "undefined"
            timeres.nodename <- paste("TemporalResolution", randomName(), sep = ".")
            graph <- add_vertices(graph,
                                  nv = 1,
                                  name = timeres.nodename,
                                  label = new.time.res,
                                  className = "ds:TemporalResolution",
                                  attr = list("ds:hasTimeStep" = time.step,
                                              "ds:hasCellMethod" = cell.method))
            graph <- add_edges(graph, 
                               c(getNodeIndexbyName(graph, cicalc.node),
                                 getNodeIndexbyName(graph, timeres.nodename)),
                               label = "ds:hasTemporalResolution")
        }
        
    }
    graph <- metaclip.graph.Command(graph, package, version, fun, arg.list,
                                    origin.node.name = cicalc.node)
    return(list("graph" = graph, "parentnodename" = cicalc.node))
}
