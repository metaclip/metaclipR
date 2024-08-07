% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metaclipR.Ensemble.R
\name{metaclipR.Ensemble}
\alias{metaclipR.Ensemble}
\title{Construct a METACLIP representation of the ds:Ensemble class}
\usage{
metaclipR.Ensemble(
  package = "transformeR",
  version = as.character(packageVersion(package)),
  output = NULL,
  fun = "bindGrid.member",
  arg.list = NULL,
  combination.method = NULL,
  graph.list,
  disable.command = FALSE,
  dc.description = NULL
)
}
\arguments{
\item{package}{package name. Default to \code{"transformeR"}}

\item{version}{Package version string.}

\item{output}{Optional. The output R object name, as character string}

\item{fun}{function name. Default to \code{"bindGrid.member"})}

\item{arg.list}{Either a character string containing a literal command call or a key-value pair list of arguments-values. See the dedicated section more for details.}

\item{combination.method}{Optional. Character string refering to the combination method used to construct the ensemble.
This is represented by the ds class "CombinationMethod", for which several individual instances exist (this
can be indicated here). Type \code{knownClassIndividuals("CombinationMethod")} for further details.}

\item{graph.list}{A \code{metaclipR} object data list, each element being the
graph defining each ensemble member}

\item{disable.command}{Better not to touch. For internal usage only (used to re-use most of the code in other
functions, but skipping command tracking)}

\item{dc.description}{Default to \code{NULL} and unused. Otherwise, this is a character string that will be appendend as a
"dc:description" annotation to the ds:Ensemble-class node.}
}
\description{
Build a directed metadata graph describing an Ensemble transformation from two or more
ds:Steps
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

\seealso{
Other transformation: 
\code{\link{metaclip.graph.Command}()},
\code{\link{metaclipR.Aggregation}()},
\code{\link{metaclipR.AnomalyCalculation}()},
\code{\link{metaclipR.Binding}()},
\code{\link{metaclipR.Climatology}()},
\code{\link{metaclipR.Dataset}()},
\code{\link{metaclipR.Interpolation}()},
\code{\link{metaclipR.etccdi}()}
}
\author{
D. San Martín, J. Bedia
}
\concept{transformation}
