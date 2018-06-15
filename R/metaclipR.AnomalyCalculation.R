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
#' @examples 
#' require(transformeR)
#' require(igraph)
#' pkg <- "transformeR"
#' v <- "1.1.1"
#' # Assume a given hindcast DatasetSubset: 
#' data("CFS_Iberia_psl")
#' # In this example it is assumed that the 1983-1990 partition is the hindcast
#' # (the reference period used to compute the anomaly), and that the 2002 subset
#' # is the operational forecast for that year. 
#' # Note that the dataset is daily:
#' getTimeResolution(CFS_Iberia_psl)
#' 
#' # First, a dataset subset of just 3 members is created as example. The DatasetSubset initializes
#' # the graph in this example
#' 
#' psl <- subsetGrid(CFS_Iberia_psl, members = 1:3)
#' metadata <- metaclipR.DatasetSubset(package = pkg,
#'                                     version = v,
#'                                     fun = "subsetGrid",
#'                                     output = "psl",
#'                                     arg.list = list("members" = 1:3))
#'                                          
#' plot.igraph(metadata$graph, vertex.size = 5, edge.arrow.size = 0.1)
#'                                          
#' # Annual data are calculated via aggregation: 
#' 
#' CFS.psl.annual <- aggregateGrid(psl,
#'                                 aggr.m = list(FUN = "mean", na.rm = TRUE),
#'                                 aggr.y = list(FUN = "mean", na.rm = TRUE))
#' # and the corresponding metadata added to the graph:
#' metadata <- metaclipR.Aggregation(graph = metadata,
#'                                   package = pkg,
#'                                   version = v,
#'                                   fun = "aggregateGrid",
#'                                   arg.list = list("aggr.m" = list(FUN = "mean", na.rm = TRUE),
#'                                                   "aggr.y" = list(FUN = "mean", na.rm = TRUE)))
#' 
#' plot.igraph(metadata$graph, vertex.size = 5, edge.arrow.size = 0.1)
#' 
#' # Next the baseline period used as reference for anomaly calculation is subset:
#' ref <- subsetGrid(CFS.psl.annual, years = 1983:1990)
#' 
#' metadata <- metaclipR.DatasetSubset(package = pkg,
#'                                     graph = metadata,
#'                                     version = v,
#'                                     fun = "subsetGrid",
#'                                     output = "ref",
#'                                     arg.list = list("years" = 1983:1990))
#'                                          
#' plot.igraph(metadata$graph, vertex.size = 5, edge.arrow.size = 0.1)
#' 
#' # The same is done for the "fake" operative forecast for 2002, 
#' #     assumed to come from a different dataset:
#' fcst <- subsetGrid(CFS.psl.annual, years = 2002)
#' # THE FORECAST IS ADDED TO A NEW GRAPH. However,
#' # no linking property between hindcast and forecast is indicated 
#' metadata2 <- metaclipR.DatasetSubset(package = pkg,
#'                                      version = v,
#'                                      arg.list = list("years" = 2002),
#'                                      output = "fcst")
#'                                           
#' plot.igraph(metadata2$graph, vertex.size = 5, edge.arrow.size = 0.1)
#' 
#' # The function localScaling is used to compute anomalies. 
#' # In its default setup, it subtracts the climatology (mean) of the input grid:
#' anom <- localScaling(grid = fcst,
#'                      base = ref,
#'                      ref = NULL,
#'                      time.frame = "none",
#'                      by.member = TRUE)
#' # This is the argument list (the data arrays are indicated as character 
#'      # strings to avoid  a congested graph):
#' arg.list = list("grid" = "fcst",
#'                 "base" = "ref",
#'                 "ref" = NULL,
#'                 "clim.fun" = list(FUN = mean, na.rm = TRUE),
#'                 "by.member" = TRUE,
#'                 "time.frame" = "none")
#' 
#' # The metadata of the anomaly calculation is next encoded. 
#' # Note that two different graphs are used as input:
#' #  i) The graph containing the steps to produce the baseline data, 
#' #         used as reference to compute the anomalies
#' #  ii) The graph containing the data upon which the anomalies are to be calculated
#' 
#' out <- metaclipR.AnomalyCalculation(graph = metadata2,
#'                                     package = pkg,
#'                                     version = v,
#'                                     fun = "localScaling",
#'                                     arg.list = arg.list,
#'                                     referenceGraph = metadata)
#' 
#' plot.igraph(out$graph, vertex.size = 5, edge.arrow.size = 0.1)

metaclipR.AnomalyCalculation <- function(graph,
                                         package = "transformeR",
                                         version = "1.3.2",
                                         fun = "localScaling",
                                         arg.list = NULL,
                                         referenceGraph = NULL) {
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
    graph <- add_vertices(graph,
                          nv = 1,
                          name = anom.nodename,
                          label = "Anomaly",
                          className = "ds:Anomaly",
                          description = "Anomaly Class",
                          attr = list("ds:hasTimeFrame" = arg.list$time.frame))
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

