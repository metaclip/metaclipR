##     metaclipR.Regridding Construct a directed graph for regridding transformation metadata encoding
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

#' @title Directed metadata graph construction for Transformations of DatasetSubsets
#' @description Build a directed metadata graph describing a regridding Transformation on a 
#' climate4R grid 
#' @param package package
#' @param version version
#' @param graph A previous metaclipR data structure from which the current step follows
#' @param fun function name. Unused (set to \code{"interpGrid"})
#' @template template_arglistParam
#' @template template_arglist
#' @details This function takes as reference the semantics defined in the Data Source and Transformation ontology
#' defined in the Metaclip Framework (\url{http://www.metaclip.org}).
#' @family transformation
#' @export
#' @importFrom igraph make_empty_graph add_vertices add_edges 
#' @author D. San Martín, J. Bedia
#' @examples 
#' require(transformeR)
#' require(igraph)
#' pkg <- "transformeR"
#' v <- "1.1.1"
#' # Assume a given hindcast DatasetSubset: 
#' data("CFS_Iberia_hus850")
#' DS <- subsetGrid(CFS_Iberia_hus850, members = 1:3, years = 1989:1991)
#' graph <- metaclipR.DatasetSubset(package = pkg,
#'                                  version = v,
#'                                  arg.list = list(members = 1:3,
#'                                                  years = 1989:1991),
#'                                  fun = "subsetGrid",
#'                                  output = "DS")
#' # Data are regridded to the regular EOBS 0.25 grid
#' data("EOBS_Iberia_tas")
#' ref.grid <- getGrid(EOBS_Iberia_tas)
#' # We apply the fast 'akima' interpolator
#' out <- interpGrid(DS,
#'                   new.coordinates = ref.grid,
#'                   method = "bilinear",
#'                   bilin.method = "akima")
#' plotClimatology(climatology(out), backdrop.theme = "coastline")
#' # This is how metadata is encoded:
#' # 1.) We identify the origin node from which the first transformation hangs:
#' graph$parentnodename
#' # 2.) Argument list
#' arg.list <- list("new.coordinates" = ref.grid,
#'                  "method" = "bilinear",
#'                  "bilin.method" = "akima")
#' # 3.) metaclipR.Regridding is called:
#' graph <- metaclipR.Regridding(package = pkg,
#'                               version = v,
#'                               graph = graph,
#'                               fun = "interpGrid",
#'                               arg.list = arg.list)
#' # This is the graph structure containing the metadata:
#' plot(graph$graph)


metaclipR.Regridding <- function(graph,
                                 package = "transformeR",
                                 version = "1.3.2",
                                 fun = "interpGrid",
                                 arg.list = NULL) {
    if (class(graph$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    if (is.null(arg.list$new.coordinates)) {
        stop("The 'new.coordinates' argument is missing in the argument list, with no default")
    }
    if (is.null(arg.list$method)) {
        stop("The 'method' argument is missing in the argument list, with no default")
    }
    if (arg.list$method == "bilinear") {
        if (is.null(arg.list$bilin.method)) {
            stop("The 'bilin.method' argument is missing in the argument list, with no default")
        }
    }
    orig.node <- graph$parentnodename
    graph <- graph$graph
    # New resolution
    resX <- if (is.null(attr(arg.list$new.coordinates, "resX"))) {
        diff(arg.list$new.coordinates$x[1])    
    } else {
        attr(arg.list$new.coordinates, "resX")
    }
    resY <- if (is.null(attr(arg.list$new.coordinates, "resY"))) {
        diff(arg.list$new.coordinates$x[1])    
    } else {
        attr(arg.list$new.coordinates, "resY")
    }
    # New spatial extent
    regnodename <- paste("Regridding", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = regnodename,
                          label = "Regridding",
                          className = "ds:Regridding",
                          description = "Regridding Class",
                          attr = list("ds:withRegriddingMethod" = arg.list$method,
                                      "ds:hasHorizontalResX" = resX,
                                      "ds:hasHorizontalResY" = resY))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, orig.node),
                         getNodeIndexbyName(graph, regnodename)),
                       label = "ds:hadRegridding")
    spatextent.nodename <- paste("SpatialExtent", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = spatextent.nodename,
                          label = "NewSpatialExtent",
                          className = "ds:SpatialExtent",
                          description = "SpatialExtent Class",
                          attr = list("ds:xmin" = arg.list$new.coordinates$x[1],
                                      "ds:xmax" = tail(arg.list$new.coordinates$x, 1),
                                      "ds:ymin" = arg.list$new.coordinates$y[1],
                                      "ds:ymax" = tail(arg.list$new.coordinates$y, 1)))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, regnodename),
                         getNodeIndexbyName(graph, spatextent.nodename)),
                       label = "ds:hasHorizontalExtent")
    # Package/Command/Argument metadata ---------------------------------------
    if ("grid" %in% names(arg.list)) arg.list <- arg.list[-grep("grid", names(arg.list))]
    graph <- metaclip.graph.Command(graph, package, version, fun, arg.list,
                                    origin.node.name = regnodename)
    return(list("graph" = graph, "parentnodename" = regnodename))
}
