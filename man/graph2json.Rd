% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph2json.R
\name{graph2json}
\alias{graph2json}
\title{RDF graph serialization to JSON-LD}
\usage{
graph2json(graph, output.file, template = FALSE)
}
\arguments{
\item{graph}{An i-graph class graph}

\item{output.file}{Character string. Output path}

\item{template}{Logical. In this case, the ouput file is a user-defined JSON template
 with a specific context and import definitions.
Unused by default (FALSE) and imports the basic METACLIP ontologies.}
}
\value{
A JSON-LD representation of the metadata structure
}
\description{
Takes an igraph-class RDF graph and write it in JSON-LD
}
\references{
A useful app to check/test JSON-LD mark <https://json-ld.org/playground/>
}
\seealso{
[prettyJSON()]

Other graphical.outputs: 
\code{\link{embedFig}()},
\code{\link{embedJSON}()}
}
\author{
J Bedia
}
\concept{graphical.outputs}
