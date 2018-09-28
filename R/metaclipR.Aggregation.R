##     metaclipR.Aggregation Construct a directed graph for Transformation metadata encoding
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

#' @title Directed metadata graph construction for Transformations of DatasetSubsets
#' @description Build a directed metadata graph describing a Transformation on a 
#' climate4R grid that is a subset of Dataset.
#' @param package package
#' @param graph A previous metaclipR data structure from which the current step follows
#' @param version version
#' @param fun function name. Unused (set to \code{"aggregateGrid"})
#' @param disable.command Better not to touch. For internal usage only (used to re-use most of the code in other
#'  functions, but skipping command tracking)
#' @param use.arg.list Logical flag. Used to optionally omit (\code{use.arg.list = FALSE}) the argument list in the description of command call. 
#' Default to TRUE.
#' @template template_arglistParam
#' @template template_arglist
#' @details This function takes as reference the semantics defined in the Data Source and Transformation ontology
#' defined in the Metaclip Framework (\url{http://www.metaclip.org}).
#' @references 
#' \href{http://www.meteo.unican.es/en/climate4r}{Climate4R page at University of Cantabria}
#' @family transformation
#' @return A named list with the updated graph in element \code{"graph"} and the parent node name (\code{"parentnodename"}),
#' needed for linking subsequent operations.
#' @export
#' @importFrom igraph make_empty_graph add_vertices add_edges 
#' @importFrom utils packageVersion
#' @author D. San Martín, J. Bedia
#' @examples \dontrun{
#' require(transformeR)
#' require(igraph)
#' pkg <- "transformeR"
#' v <- "1.1.1"
#' # Assume a given hindcast DatasetSubset (we first simplify a)
#' data("CFS_Iberia_hus850")
#' DS <- subsetGrid(CFS_Iberia_hus850, members = 1:3, years = 1989:1991)
#' graph <- metaclipR.DatasetSubset(package = pkg,
#'                                  version = v,
#'                                  arg.list = list(members = 1:3,
#'                                                  years = 1989:1991),
#'                                  fun = "subsetGrid",
#'                                  output = "DS")
#' # An aggregation is performed on the example data. In this case, 
#' # The forecast is aggregated by members (ensemble mean), and in space
#' # (along longitude and latitude) to obtain a spatial mean. Original data is daily,
#' # and an annual aggregation is performed, considering the monthly means and the
#' # annual maxima:
#' fun <- "aggregateGrid"
#' arg.list <- list("aggr.mem" = list(FUN = "mean", na.rm = TRUE),
#'                  "aggr.d" = list(FUN = NULL),
#'                  "aggr.m" = list(FUN = "mean", na.rm = TRUE),
#'                  "aggr.y" = list(FUN = "max", na.rm = TRUE),
#'                  "aggr.lat" = list(FUN = "mean", na.rm = TRUE),
#'                  "aggr.lon" = list(FUN = "mean", na.rm = TRUE),
#'                  "weight.by.lat" = TRUE)
#' # The aggregation is undertaken with transformeR::subsetGrid:
#' arg.list[["grid"]] <- DS
#' out <- do.call(fun, arg.list)
#' # This is how metadata is encoded:
#' # 1.) We identify the origin node from which the first transformation hangs:
#' graph$parentnodename
#' # 2.) metaclipR.Aggregation is called:
#' graph <- metaclipR.Aggregation(package = pkg,
#'                                version = v,
#'                                graph = graph,
#'                                fun = fun,
#'                                arg.list = arg.list)
#' # And this is the metadata description stored in the igraph-class object:
#' plot(graph$graph)
#' }

metaclipR.Aggregation <- function(package = "transformeR",
                                  version = as.character(packageVersion(package)),
                                  graph,
                                  fun = "aggregateGrid",
                                  arg.list = NULL,
                                  use.arg.list = TRUE,
                                  disable.command = FALSE) {
    orig.node <- graph$parentnodename
    graph <- graph$graph
    if (class(graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    # Aggregation -----------------------------------------------
    aggr.nodename <- paste("Aggregation", randomName(), sep = ".")
    orig.nodes.command <- c()
    orig.nodes.command <- c(orig.nodes.command, aggr.nodename)
    graph <- add_vertices(graph,
                          nv = 1,
                          name = aggr.nodename,
                          label = "Aggregation",
                          className = "ds:Aggregation",
                          description = "Aggregation Class")
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, orig.node),
                         getNodeIndexbyName(graph, aggr.nodename)),
                       label = "ds:hadAggregation")
    # Member aggregation -----------------------------------------------
    if (!is.null(arg.list$aggr.mem$FUN)) {
        cellme <- paste(deparse(arg.list$aggr.mem$FUN), collapse = "")
        cellme <- gsub("\"","'", cellme)
        aggr.mem.nodename <- paste("Member", randomName(), sep = ".")
        graph <- add_vertices(graph,
                              nv = 1,
                              name = aggr.mem.nodename,
                              label = "Member",
                              className = "ds:Member",
                              description = "Member Class",
                              attr = list("ds:hasCellMethod" = cellme))
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, aggr.nodename),
                             getNodeIndexbyName(graph, aggr.mem.nodename)),
                           label = "ds:alongDimension")
    }
    # Lon aggregation ---------------------------------------------------------
    if (!is.null(arg.list$aggr.lon$FUN)) {
        cellme <- paste(deparse(arg.list$aggr.lon$FUN), collapse = "")
        cellme <- gsub("\"","'", cellme)
        aggr.lon.nodename <- paste("Longitude", randomName(), sep = ".")
        graph <- add_vertices(graph,
                              nv = 1,
                              name = aggr.lon.nodename,
                              label = "Longitude",
                              className = "ds:Longitude",
                              description = "Longitude Class",
                              attr = list("ds:hasCellMethod" = cellme))
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, aggr.nodename),
                             getNodeIndexbyName(graph, aggr.lon.nodename)),
                           label = "ds:alongDimension")
    }
    # Lat aggregation ---------------------------------------------------------
    if (!is.null(arg.list$aggr.lat$FUN)) {
        cellme <- paste(deparse(arg.list$aggr.lat$FUN), collapse = "")
        cellme <- gsub("\"","'", cellme)
        aggr.lat.nodename <- paste("Latitude", randomName(), sep = ".")
        graph <- add_vertices(graph,
                              nv = 1,
                              name = aggr.lat.nodename,
                              label = "Latitude",
                              className = "ds:Latitude",
                              description = "Latitude Class",
                              attr = list("ds:hasCellMethod" = cellme))
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, aggr.nodename),
                             getNodeIndexbyName(graph, aggr.lat.nodename)),
                           label = "ds:alongDimension")
    }
    # Subdaily-to-daily aggregation -------------------------------------------
    time.counter <- c()
    if (!is.null(arg.list$aggr.d$FUN)) {
        time.counter <- c(time.counter, 1)
        cellme <- paste(deparse(arg.list$aggr.d$FUN), collapse = "")
        cellme <- gsub("\"","'", cellme)
        aggr.time.nodename <- paste("ValidationTime", randomName(), sep = ".")
        graph <- my_add_vertices(graph,
                                 name = aggr.time.nodename,
                                 label = "ValidationTime",
                                 className = "ds:ValidationTime",
                                 attr = list("ds:hasCellMethod" = cellme))
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, aggr.nodename),
                             getNodeIndexbyName(graph, aggr.time.nodename)),
                           label = "ds:alongDimension")
        # Update temporal Resolution
        timeres.nodename <- paste("TemporalResolution", randomName(), sep = ".")
        graph <- my_add_vertices(graph,
                                 name = timeres.nodename,
                                 label = "DailyResolution",
                                 className = "ds:TemporalResolution",
                                 attr = list("ds:hasTimeStep" = "P1D",
                                             "ds:hasCellMethod" = cellme))
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, aggr.nodename),
                             getNodeIndexbyName(graph, timeres.nodename)),
                           label = "ds:hasTemporalResolution")
    }
    # Daily to monthly aggregation -------------------------------------------
    if (!is.null(arg.list$aggr.m$FUN)) {
        cellme <- paste(deparse(arg.list$aggr.m$FUN), collapse = "")
        cellme <- gsub("\"","'", cellme)
        time.counter <- c(time.counter, 1)
        if (sum(time.counter) > 1) {
            aggr.m.nodename <- paste("Aggregation.monthly", randomName(), sep = ".")
            graph <- my_add_vertices(graph,
                                     name = aggr.m.nodename,
                                     label = "MonthlyAggregation",
                                     className = "ds:Aggregation")
            graph <- add_edges(graph, 
                               c(getNodeIndexbyName(graph, aggr.nodename),
                                 getNodeIndexbyName(graph, aggr.m.nodename)),
                               label = "ds:hadAggregation")
            aggr.nodename <- aggr.m.nodename
            orig.nodes.command <- c(orig.nodes.command, aggr.nodename)
        }
        aggr.time.nodename <- paste("ValidationTime", randomName(), sep = ".")
        graph <- my_add_vertices(graph,
                                 name = aggr.time.nodename,
                                 label = "ValidationTime",
                                 className = "ds:ValidationTime",
                                 attr = list("ds:hasCellMethod" = cellme))
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, aggr.nodename),
                             getNodeIndexbyName(graph, aggr.time.nodename)),
                           label = "ds:alongDimension")
        # Update temporal Resolution
        timeres.nodename <- paste("TemporalResolution", randomName(), sep = ".")
        graph <- my_add_vertices(graph,
                                 name = timeres.nodename,
                                 label = "MonthlyResolution",
                                 className = "ds:TemporalResolution",
                                 attr = list("ds:hasTimeStep" = "P1M",
                                             "ds:hasCellMethod" = cellme))
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, aggr.nodename),
                             getNodeIndexbyName(graph, timeres.nodename)),
                           label = "ds:hasTemporalResolution")
    }
    # Monthly to seasonal/annual aggregation -------------------------------------------
    if (!is.null(arg.list$aggr.y$FUN)) {
        cellme <- paste(deparse(arg.list$aggr.y$FUN), collapse = "")
        cellme <- gsub("\"","'", cellme)
        time.counter <- c(time.counter, 1)
        if (sum(time.counter) > 1) {
            aggr.y.nodename <- paste("Aggregation.yearly", randomName(), sep = ".")
            graph <- my_add_vertices(graph,
                                     name = aggr.y.nodename,
                                     label = "AnnualAggregation",
                                     className = "ds:Aggregation")
            graph <- add_edges(graph, 
                               c(getNodeIndexbyName(graph, aggr.nodename),
                                 getNodeIndexbyName(graph, aggr.y.nodename)),
                               label = "ds:hadAggregation")
            aggr.nodename <- aggr.y.nodename
            orig.nodes.command <- c(orig.nodes.command, aggr.nodename)
        }
        aggr.time.nodename <- paste("ValidationTime", randomName(), sep = ".")
        graph <- my_add_vertices(graph,
                                 name = aggr.time.nodename,
                                 label = "ValidationTime",
                                 className = "ds:ValidationTime",
                                 attr = list("ds:hasCellMethod" = cellme))
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, aggr.nodename),
                             getNodeIndexbyName(graph, aggr.time.nodename)),
                           label = "ds:alongDimension")
        # Update temporal Resolution
        timeres.nodename <- paste("TemporalResolution", randomName(), sep = ".")
        graph <- my_add_vertices(graph,
                                 name = timeres.nodename,
                                 label = "AnnualResolution",
                                 className = "ds:TemporalResolution",
                                 attr = list("ds:hasTimeStep" = "P1Y",
                                             "ds:hasCellMethod" = cellme))
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, aggr.nodename),
                             getNodeIndexbyName(graph, timeres.nodename)),
                           label = "ds:hasTemporalResolution")
    }
    # Package/Command/Argument metadata ---------------------------------------
    if (!disable.command) {
        if ("grid" %in% names(arg.list)) arg.list <- arg.list[-grep("grid", names(arg.list))]
        if (!use.arg.list) names(arg.list) <- NULL
        graph <- metaclip.graph.Command(graph, package, version, fun, arg.list,
                                        origin.node.name = orig.nodes.command)
    }
    return(list("graph" = graph,
                "parentnodename" = tail(orig.nodes.command, 1))
    ) 
}
