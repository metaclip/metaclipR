% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metaclipR.Interpolation.R
\name{metaclipR.Interpolation}
\alias{metaclipR.Interpolation}
\title{Directed metadata graph construction for interpolation Transformations}
\usage{
metaclipR.Interpolation(
  graph,
  package = "transformeR",
  version = as.character(packageVersion(package)),
  RefSpatialExtent = NULL,
  InterpolationMethod,
  fun = "interpGrid",
  arg.list = NULL,
  disable.command = FALSE,
  dc.description = NULL
)
}
\arguments{
\item{graph}{A previous metaclipR data structure from which the current step follows}

\item{package}{package}

\item{version}{version}

\item{RefSpatialExtent}{A reference spatial extent used for interpolation. The reference spatial extent can be initiated with \code{\link{metaclipR.SpatialExtent}}}

\item{InterpolationMethod}{Interpolation method. Current possible choices include \code{"nearest"}, \code{"bilinear"}, \code{"bicubic"},
\code{"IDW"}, \code{"spline"} and \code{"ConservativeRemapping"}, but these will be probably updated in the future to accommodate further methods.}

\item{fun}{function name. Unused (set to \code{"interpGrid"})}

\item{arg.list}{Either a character string containing a literal command call or a key-value pair list of arguments-values. See the dedicated section more for details.}

\item{disable.command}{Better not to touch. For internal usage only (used to re-use most of the code in other
functions, but skipping command tracking)}

\item{dc.description}{Default to \code{NULL} and unused. Otherwise, this is a character string that will be appendend as a
"dc:description" annotation to the ds:Interpolation-class node.}
}
\description{
Build a directed metadata graph describing an interpolation
}
\details{
This function takes as reference the semantics defined in the Data Source and Transformation ontology
defined in the Metaclip Framework (\url{http://www.metaclip.org}).
}
\section{About argument lists}{

Argument lists are key-value list in which the different arguments and their valus are passed 
to the command description. These were initially conceived as an adequate means of describing
command calls to interpreted languajes (R, python...) using both the Argument and Argument 
classes from the datasource vocabulary of METACLIP.

Alternatively, and in order to accomodate the description of the source code to other environments
and languajes (e.g. shell scripts, calls to CDOs etc.), the use of literal command calls 
is a more convenient choice. Literal command calls also allow for a more straightforward
reproducibility by just "copying and pasting" the code used to generate each step. Literal
command calls are encoded in METACLIP using the \emph{hadLiteralCommandCall} data property
from the datasource vocabulary, that is attached to the command description. 
To apply this second alternative, the \code{arg.list} argument is specified as a character string
containing the command call.

Note that default argument values are often omitted from comand calls. However, the default behaviour of 
commands may change depending on the specific software version, and it is therefore often advisable to 
explicitly include the default argument values for a more straightforward interpretation of the source code
by the user.
}

\examples{
\dontrun{
require(transformeR)
require(igraph)
pkg <- "transformeR"
# Assume a given hindcast DatasetSubset: 
data("CFS_Iberia_hus850")
DS <- subsetGrid(CFS_Iberia_hus850, members = 1:3, years = 1989:1991)
comcall <- "subsetGrid(CFS_Iberia_hus850, members = 1:3, years = 1989:1991)"
graph <- metaclipR.DatasetSubset(package = pkg,
                                 arg.list = comcall,
                                 fun = "subsetGrid",
                                 output = "DS")
# Data are regridded to the regular EOBS 0.25 grid
data("EOBS_Iberia_tas")
ref.grid <- getGrid(EOBS_Iberia_tas)
refSp <- metaclipR.SpatialExtent(EOBS_Iberia_tas$xyCoords)
# We apply the fast 'akima' interpolator
out <- interpGrid(DS,
                  new.coordinates = ref.grid,
                  method = "bilinear",
                  bilin.method = "akima")
comcall <- "interpGrid(DS,
                       new.coordinates = ref.grid,
                       method = \"bilinear\",
                       bilin.method = \"akima\")"
require(visualizeR)
spatialPlot(climatology(out), backdrop.theme = "coastline")
# This is how metadata is encoded:
# metaclipR.Interpolation is called, using as reference the grid from E-OBS
graph <- metaclipR.Interpolation(package = pkg, 
                                 graph = graph,
                                 InterpolationMethod = "bilinear",
                                 fun = "interpGrid",
                                 RefSpatialExtent = refSp,
                                 arg.list = comcall)
# This is the graph structure containing the metadata:
plot(graph$graph)
}
}
\seealso{
Other transformation: 
\code{\link{metaclip.graph.Command}()},
\code{\link{metaclipR.Aggregation}()},
\code{\link{metaclipR.AnomalyCalculation}()},
\code{\link{metaclipR.Binding}()},
\code{\link{metaclipR.Climatology}()},
\code{\link{metaclipR.Dataset}()},
\code{\link{metaclipR.Ensemble}()},
\code{\link{metaclipR.etccdi}()}
}
\author{
D. San Martín, J. Bedia
}
\concept{transformation}
