##     metaclipR.Interpolation Construct a directed graph for interpolation operations
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

#' @title Directed metadata graph construction for interpolation Transformations 
#' @description Build a directed metadata graph describing an interpolation
#' @param package package
#' @param version version
#' @param graph A previous metaclipR data structure from which the current step follows
#' @param RefSpatialExtent A reference spatial extent used for interpolation. The reference spatial extent can be initiated with \code{\link{metaclipR.SpatialExtent}}
#' @param fun function name. Unused (set to \code{"interpGrid"})
#' @param InterpolationMethod Interpolation method. Current possible choices include \code{"nearest"}, \code{"bilinear"}, \code{"bicubic"},
#'  \code{"IDW"}, \code{"spline"} and \code{"ConservativeRemapping"}, but these will be probably updated in the future to accommodate further methods.
#' @param disable.command Better not to touch. For internal usage only (used to re-use most of the code in other
#'  functions, but skipping command tracking)
#' @param dc.description Default to \code{NULL} and unused. Otherwise, this is a character string that will be appendend as a
#'  "dc:description" annotation to the ds:Interpolation-class node.
#' @template template_arglistParam
#' @template template_arglist
#' @details This function takes as reference the semantics defined in the Data Source and Transformation ontology
#' defined in the Metaclip Framework (\url{http://www.metaclip.org}).
#' @family transformation
#' @export
#' @importFrom igraph make_empty_graph add_vertices add_edges 
#' @importFrom utils packageVersion
#' @author D. San Martín, J. Bedia
#' @examples \dontrun{
#' require(transformeR)
#' require(igraph)
#' pkg <- "transformeR"
#' # Assume a given hindcast DatasetSubset: 
#' require(climate4R.datasets)
#' data("CFS_Iberia_hus850")
#' DS <- subsetGrid(CFS_Iberia_hus850, members = 1:3, years = 1989:1991)
#' comcall <- "subsetGrid(CFS_Iberia_hus850, members = 1:3, years = 1989:1991)"
#' graph <- metaclipR.DatasetSubset(package = pkg,
#'                                  arg.list = comcall,
#'                                  fun = "subsetGrid",
#'                                  output = "DS")
#' # Data are regridded to the regular EOBS 0.25 grid
#' data("EOBS_Iberia_tas")
#' ref.grid <- getGrid(EOBS_Iberia_tas)
#' refSp <- metaclipR.SpatialExtent(EOBS_Iberia_tas$xyCoords)
#' # We apply the fast 'akima' interpolator
#' out <- interpGrid(DS,
#'                   new.coordinates = ref.grid,
#'                   method = "bilinear",
#'                   bilin.method = "akima")
#' comcall <- "interpGrid(DS,
#'                        new.coordinates = ref.grid,
#'                        method = \"bilinear\",
#'                        bilin.method = \"akima\")"
#' require(visualizeR)
#' spatialPlot(climatology(out), backdrop.theme = "coastline")
#' # This is how metadata is encoded:
#' # metaclipR.Interpolation is called, using as reference the grid from E-OBS
#' graph <- metaclipR.Interpolation(package = pkg, 
#'                                  graph = graph,
#'                                  InterpolationMethod = "bilinear",
#'                                  fun = "interpGrid",
#'                                  RefSpatialExtent = refSp,
#'                                  arg.list = comcall)
#' # This is the graph structure containing the metadata:
#' plot(graph$graph)
#' }

metaclipR.Interpolation <- function(graph,
                                    package = "transformeR",
                                    version = as.character(packageVersion(package)),
                                    RefSpatialExtent = NULL,
                                    InterpolationMethod,
                                    fun = "interpGrid",
                                    arg.list = NULL,
                                    disable.command = FALSE,
                                    dc.description = NULL) {
    if (class(graph$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    # if (is.list(arg.list)) { 
    #     stop("The use of an argument list in this function has been deprecated since v1.1.0")
    # }
    stopifnot(is.logical(disable.command))
    interp.method <- match.arg(InterpolationMethod, choices = c("nearest", "bilinear", "bicubic", "IDW", "spline", "conservative"))
    interp.method.class <- switch(interp.method,
                                  "nearest" = "ds:NearestNeighbor",
                                  "bilinear" = "ds:BilinearInterpolation",
                                  "bicubic" = "ds:BicubicInterpolation",
                                  "IDW" = "ds:InverseDistanceWeighting",
                                  "spline" = "ds:Splines",
                                  "conservative" = "ds:ConservativeRemapping")
    orig.node <- graph$parentnodename
    graph <- graph$graph
    # New spatial extent
    regnodename <- paste("Interpolation", randomName(), sep = ".")
    if (is.null(dc.description)) {
        graph <- add_vertices(graph,
                              nv = 1,
                              label = paste(interp.method, "interpolation"),
                              name = regnodename,
                              className = "ds:Interpolation")
    } else {
        graph <- add_vertices(graph,
                              nv = 1,
                              label = paste(interp.method, "interpolation"),
                              name = regnodename,
                              className = "ds:Interpolation",
                              attr = list("dc:description" = dc.description))
    }
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, orig.node),
                         getNodeIndexbyName(graph, regnodename)),
                       label = "ds:hadInterpolation")
    regmethod.nodename <- paste("InterpolationMethod", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = regmethod.nodename,
                          label = interp.method,
                          className = interp.method.class)
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, regnodename),
                         getNodeIndexbyName(graph, regmethod.nodename)),
                       label = "ds:hadInterpolationMethod")
    # Link SpatialExtent
    if (!is.null(RefSpatialExtent)) {
        if (class(RefSpatialExtent$graph) != "igraph") stop("Invalid \'RefSpatialExtent\' structure")
        spatextent.nodename <- RefSpatialExtent$parentnodename
        graph <- my_union_graph(graph, RefSpatialExtent$graph)
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, regnodename),
                             getNodeIndexbyName(graph, spatextent.nodename)),
                           label = "ds:usedReferenceCoordinates")
    }
    # Package/Command/Argument metadata ---------------------------------------
    if (!disable.command) {
        if ("grid" %in% names(arg.list)) arg.list <- arg.list[-grep("grid", names(arg.list))]
        graph <- metaclip.graph.Command(graph, package, version, fun, arg.list,
                                        origin.node.name = regnodename)
    }
    return(list("graph" = graph, "parentnodename" = regnodename))
}





#' @title Directed metadata graph construction for Transformations of DatasetSubsets
#' @description Build a directed metadata graph describing a regridding Transformation on a 
#' climate4R grid 
#' @param package package
#' @param version version
#' @param graph A previous metaclipR data structure from which the current step follows
#' @param fun function name. Unused (set to \code{"interpGrid"})
#' @param arg.list Argument list. See details
#' @details This function takes as reference the semantics defined in the Data Source and Transformation ontology
#' defined in the Metaclip Framework (\url{http://metaclip.predictia.es/}).
#' 
#' \strong{Argument list}
#' 
#' The following list of arguments is required to define an aggregation:
#' \itemize{
#' \item \code{new.coordinates}
#' \item \code{method}
#' \item \code{bilin.method}
#' }
#' 
#' The different arguments are explained in the the help page of \code{\link[transformeR]{interpGrid}}. 
#' 
#' @references 
#' \href{http://www.meteo.unican.es/en/climate4r}{Climate4R page at University of Cantabria}
#' \url{metaclip.predictia.es}
#' @export
#' @importFrom igraph make_empty_graph add_vertices add_edges 
#' @author D. San Martín, J. Bedia


metaclipR.Regridding <- function(graph,
                                 package = "transformeR",
                                 version = "1.3.2",
                                 fun = "interpGrid",
                                 arg.list = NULL) {
    .Deprecated(new = "metaclipR.Interpolation")
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

