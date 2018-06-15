##     metaclipR.DatasetSubset Construct a directed graph for DatasetSubset metadata encoding
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

#' @title Directed metadata graph construction for DatasetSubsets
#' @description Build a directed metadata graph from climate4R grids that are subsets of Datasets
#' @param package package
#' @param version version
#' @param graph A previously initialized metaclipR output list ($graph + $parentnodename). Default to \code{NULL}, which starts a new empty graph
#' @param fun function name (default, and only value accepted is \code{"subsetGrid"})
#' @template template_arglistParam
#' @template template_arglist
#' @param output A climate4R grid, resulting from the application of \code{\link[transformeR]{subsetGrid}}.
#' @param disable.command Better not to touch. For internal usage only (used to re-use most of the code in other functions, but skipping command tracking)
#' @details This function takes as reference the semantics defined in the Data Source and Transformation ontology
#' defined in the Metaclip Framework (\url{http://www.metaclip.org/}).
#' 
#' @return A named list with the updated graph in element \code{"graph"} and the parent node name,
#' sometimes needed for linking subsequent operations.
#' 
#' 
#' @references 
#' \href{http://www.meteo.unican.es/en/climate4r}{Climate4R page at University of Cantabria}
#' @export
#' @importFrom igraph make_empty_graph add_vertices add_edges 
#' @importFrom transformeR getShape getSeason getRefDates getGrid getVarNames getGridUnits
#' @importFrom utils tail
#' @author D. San Martín, J. Bedia
#' @family subsetting
#' @examples 
#' data(S4_tas_iberia)
#' require(transformeR)
#' require(igraph)
#' # An example subset from S4_tas_iberia
#' arg.list <- list(grid = S4_tas_iberia,
#'                  members = 1:3, # First three members (out of 15)
#'                  season = NULL,
#'                  years = 1991:1995, # Period 1991-1995
#'                  lonLim = c(-5,3), # longitudinal extent
#'                  latLim = c(37,43),
#'                  drop = FALSE) # latitudinal extent
#' # Dataset subset is applied using subsetGrid:
#' dataset.subset <- do.call("subsetGrid", args = arg.list)
#' # Just for illustration, visualization of the climatology:
#' plotClimatology(climatology(dataset.subset, by.member = FALSE),
#'                  backdrop.theme = "countries")
#' # Encoding provenance: 
#' dsgraph <- metaclipR.DatasetSubset(package = "transformeR",
#'                                    version = "1.1.1",
#'                                    fun = "subsetGrid",
#'                                    arg.list = arg.list,
#'                                    output = "dataset.subset")
#' # The Graph containing the metadata has been created.
#' # This is a bit congested, but note that it is not conceived to be visualized in R
#' plot(dsgraph$graph, vertex.size = 5, edge.arrow.size=0.1)

metaclipR.DatasetSubset <- function(package = "transformeR",
                                    version = "1.3.3",
                                    graph = NULL,
                                    fun = NULL,
                                    arg.list = NULL,
                                    output = NULL,
                                    disable.command = FALSE) {
    # First, an empty graph is created
    emptygraph <- TRUE
    if (is.null(graph)) {
        graph <- make_empty_graph(directed = TRUE)
    } else {
        parent.node <- graph$parentnodename
        graph <- graph$graph
        if (class(graph) != "igraph") {
            stop("The input graph has not a valid format")
        }
        emptygraph <- FALSE
    }
    if (is.character(output)) output <- get(output)
    output.shape <- getShape(output)
    output$Data <- NULL
    # dataset <- attr(output, "dataset")
    # DSname <- paste(dataset, "subset", sep = ".")
    DatasetSubset.nodename <- paste0("DatasetSubset.", randomName())
    graph <- add_vertices(graph,
                          nv = 1,
                          name = DatasetSubset.nodename,
                          label = "DatasetSubset",
                          className = "ds:DatasetSubset")
    # Link with parent node if existing
    if (!emptygraph) {
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, parent.node),
                             getNodeIndexbyName(graph, DatasetSubset.nodename)),
                           label = paste0("ds:hadDatasetSubset"))    
    }
    # Variable  ---------------------
    var.name <- getVarNames(output, "long")
    shortname <- getVarNames(output, "short")
    units <- getGridUnits(output)
    vlevel <- ifelse(is.null(output$Variable$level), -9999, output$Variable$level)
    grd <- getGrid(output)
    var.nodename <- paste("Variable", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = var.nodename,
                          label = var.name,
                          className = "ds:Variable",
                          description = "Encode metadata of a Variable",
                          attr = list("ds:withUnits" = units,
                                      "ds:hasShortName" = shortname,
                                      "ds:hasVerticalLevel" = vlevel,
                                      "ds:hasHorizontalResX" = attr(grd, "resX"),
                                      "ds:hasHorizontalResY" = attr(grd, "resY")))
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, DatasetSubset.nodename),
                         getNodeIndexbyName(graph, var.nodename)),
                       label = "ds:hasVariable")
    # TemporalResolution ---------------------
    # hasTimeStep
    time.step <- getHasTimeStep(output)
    # hasCellMethod - Recovers the cell method from the last aggregation level
    ind <- tail(grep("cellfun", names(attributes(output$Variable))), 1)
    cell.method <- if (!is.null(ind)) {
        attributes(output$Variable)[[ind]]
    } else {
        "undefined"
    }
    timeres.nodename <- paste("TemporalResolution", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = timeres.nodename,
                          label = "TemporalResolution",
                          className = "ds:TemporalResolution",
                          description = "Encode metadata of temporal resolution",
                          attr = list("ds:hasTimeStep" = time.step,
                                      "ds:hasCellMethod" = cell.method))
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, DatasetSubset.nodename),
                         getNodeIndexbyName(graph, timeres.nodename)),
                       label = "ds:hasTemporalResolution")
    # VariableStandardDefinition -------------------------------- 
    # This is controlled by the dictionary
    if (attr(output$Variable, "use_dictionary")) {
        main.url <- "http://cfconventions.org/standard-names.html"
        version.tag <- "v46"
    } else {
        main.url <- "NULL"
        version.tag <- "NULL"
    }
    varstdef.nodename <- paste("VarStdDef", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = varstdef.nodename,
                          label = "StandardDef",
                          className = "ds:VariableStandardDefinition",
                          description = "Encode metadata of Variable Standard Definition (if any)",
                          attr = list("ds:hasMainURL" = main.url,
                                      "ds:withVersionTag" = as.character(version.tag)))
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, var.nodename),
                         getNodeIndexbyName(graph, varstdef.nodename)),
                       label = "ds:hasStandardDefinition")
    # SpatialExtent (Horizontal) -------------------------------------
    hext.nodename <- paste("HorizontalExtent", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = hext.nodename,
                          label = "HorizontalExtent",
                          className = "ds:SpatialExtent",
                          description = "Encode metadata of (Horizontal) Spatial Extents",
                          attr = list("ds:xmin" = grd$x[1],
                                      "ds:ymin" = grd$y[1],
                                      "ds:xmax" = grd$x[2],
                                      "ds:ymax" = grd$y[2])
                          )
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, DatasetSubset.nodename),
                         getNodeIndexbyName(graph, hext.nodename)),
                       label = "ds:hasHorizontalExtent")
    # Spatial Extent (Vertical) --------------------------------------
    vextent.nodename <- paste("VerticalExtent", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = vextent.nodename,
                          label = "VerticalExtent",
                          className = "ds:SpatialExtent",
                          description = "Encode metadata of (Vertical) Spatial Extents")
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, DatasetSubset.nodename),
                         getNodeIndexbyName(graph, vextent.nodename)),
                       label = "ds:hasVerticalExtent")
    # Realization ----------------------------------------------------
    if ("member" %in% names(output.shape)) {
        n.mem <- output.shape["member"]
        member.node.names <- vector("list", n.mem)
        for (i in 1:n.mem) {
            mem <- output$Members[i]
            if (is.null(mem)) mem <- i
            member.node.names[[i]] <- paste("Realization", i, randomName(), sep = ".")
            graph <- add_vertices(graph,
                                  nv = 1,
                                  name = member.node.names[[i]],
                                  label = mem,
                                  className = "ds:Realization",
                                  description = "Encode metadata of Realizations",
                                  attr = list("ds:hasMember" = mem))
            graph <- add_edges(graph, 
                               c(getNodeIndexbyName(graph, DatasetSubset.nodename),
                                 getNodeIndexbyName(graph, member.node.names[[i]])),
                               label = "ds:hasRealization")
        }    
        # TemporalInstant (initialization) -----------------------------------------------
        # First check if initialization times are defined for the dataset
        if (!is.null(output$InitializationDates)) {
            n.inits <- length(output$InitializationDates[[1]])
            for (i in 1:n.mem) {
                node.orig <- member.node.names[[i]]
                for (j in 1:n.inits) {
                    node.dest <- paste0(node.orig, "_init.", j)
                    init.date <- output$InitializationDates[[i]][[j]] %>% as.POSIXlt(tz = "GMT") %>% 
                        as.POSIXct() %>% format(format = "%Y-%m-%d %H:%M:%S", usetz = TRUE)
                    graph <- add_vertices(graph,
                                          nv = 1,
                                          name = node.dest,
                                          label = init.date, #paste0("InitTime_", j),
                                          className = "ds:TemporalInstant",
                                          description = "Encode metadata for Initialization Times",
                                          attr = list("prov:generatedAtTime" = init.date))
                    graph <- add_edges(graph, 
                                       c(getNodeIndexbyName(graph, node.orig),
                                         getNodeIndexbyName(graph, node.dest)),
                                       label = "ds:hasInitializationTime")
                }
            }
        }
    }
    # TemporalPeriod -----------------------------------------------
    filter.month <- as.list(getSeason(output))
    names(filter.month) <- paste("ds:filterMonth", 1:length(filter.month), sep = ".")
    startTime <- getRefDates(output)[1]  %>% as.POSIXlt(tz = "GMT") %>% 
        as.POSIXct() %>% format(format = "%Y-%m-%d %H:%M:%S", usetz = TRUE)
    endTime <- tail(getRefDates(output, "end"), 1)  %>% as.POSIXlt(tz = "GMT") %>% 
        as.POSIXct() %>% format(format = "%Y-%m-%d %H:%M:%S", usetz = TRUE)
    timeper.nodename <- paste("TemporalPeriod", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = timeper.nodename,
                          label = "TemporalPeriod",
                          className = "ds:TemporalPeriod",
                          description = "Encode metadata of Temporal Periods",
                          attr = c(
                              list("prov:startedAtTime" = startTime,
                                   "prov:endedAtTime" = endTime),
                              filter.month))
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, DatasetSubset.nodename),
                         getNodeIndexbyName(graph, timeper.nodename)),
                       label = "ds:hasValidTemporalExtent")
    # ARGUMENT - COMMAND METADATA ----------------------------------------------
    if (!disable.command) {
        if ("grid" %in% names(arg.list)) arg.list <- arg.list[-grep("grid", names(arg.list))]
        graph <- metaclip.graph.Command(graph, package, version, fun, arg.list, DatasetSubset.nodename)
    }
    return(list("graph" = graph, "parentnodename" = DatasetSubset.nodename))
}

