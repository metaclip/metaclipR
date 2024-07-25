##     graph2json Serialize an i-graph containing METACLIP RDF definition
##
##     Copyright (C) 2024 Santander Meteorology Group <https://github.com/SantanderMetGroup>
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

#' @title RDF graph serialization to JSON-LD
#' @description Takes an igraph-class RDF graph and write it in JSON-LD
#' @param graph An i-graph class graph
#' @param output.file Character string. Output path 
#' @param template Logical. In this case, the ouput file is a user-defined JSON template
#'  with a specific context and import definitions.
#' Unused by default (FALSE) and imports the basic METACLIP ontologies.
#' @return A JSON-LD representation of the metadata structure
#' @importFrom igraph V vertex_attr E edge_attr head_of is_igraph
#' @export
#' @family graphical.outputs
#' @author J Bedia
#' @seealso [prettyJSON()]
#' @references A useful app to check/test JSON-LD mark <https://json-ld.org/playground/>  


graph2json <- function(graph, output.file, template = FALSE) {
    if (!is_igraph(graph)) stop("'graph' is not an igraph-class object")
    stopifnot(is.logical(template))
    message("[", Sys.time(), "] Writing file...") 
    g <- graph
    f <- output.file 
    if (!template) {
        
        z <- file(f, "w")
        
        cat(c("{","\n"), sep = "", file = z)
        cat(c("\t\"@context\": {", "\n"), sep = "", file = z)    
        # Metaclip imports
        cat(c("\t\t\"ds\": ", "\"http://www.metaclip.org/datasource/datasource.owl#\",\n"), 
            sep = "", file = z)    
        cat(c("\t\t\"ipcc\": ", "\"http://www.metaclip.org/ipcc_terms/ipcc_terms.owl#\",\n"), 
            sep = "", file = z)    
        cat(c("\t\t\"veri\": ", "\"http://www.metaclip.org/verification/verification.owl#\",\n"), 
            sep = "", file = z)    
        cat(c("\t\t\"cal\": ", "\"http://www.metaclip.org/calibration/calibration.owl#\",\n"), 
            sep = "", file = z)    
        cat(c("\t\t\"go\": ", "\"http://www.metaclip.org/graphical_output/graphical_output.owl#\",\n"), 
            sep = "", file = z)    
        # prov-o
        cat(c("\t\t\"prov\": ", "\"http://www.w3.org/ns/prov#\",\n"),
            sep = "", file = z)
        # rdf schema
        cat(c("\t\t\"rdfs\": ", "\"http://www.w3.org/2000/01/rdf-schema#\",\n"),
            sep = "", file = z)    
        # dublin core
        cat(c("\t\t\"dc\": ", "\"http://www.w3.org/2002/07/owl\",\n"),
            sep = "", file = z)   
        # skos
        cat(c("\t\t\"skos\": ", "\"http://www.w3.org/2004/02/skos/core#\"\n"),
            sep = "", file = z)    
        cat("\t},", file = z)
        
    } else {
        z <- file(f, "a")
    }
    
    cat("\n\t\"@graph\":[", file = z)
    # Todos los vertices del grafo
    vertices <- V(g)
    # Counter de las clases ya descritas
    describedNodeNames <- c()
    firstParentNode = TRUE
    for (i in 1:length(vertices)) {
        vid <- vertex_attr(g, name = "name", index = vertices[i])
        if (!vid %in% describedNodeNames) {
            if (!firstParentNode) {
                cat("\n,", file = z)
            }
            cat("{", file = z)
            describedNodeNames <- serializeVertex(g, vertex = vertices[i], describedNodeNames, connection = z)            
            cat("}", file = z)
            firstParentNode = FALSE
        }
    }
    cat("]}", file = z)
    cat("\n", file = z)
    close(z)
    message("[", Sys.time(), "] The file '", output.file, "' was sucessfuly generated")    
}


#' @title recursive function for graph serialization        
#' @description Recursive function to trace and encode properties of nodes 
#' @keywords internal
#' @importFrom Rgraphviz from

serializeVertex <- function(g, vertex, describedNodeNames, connection) {
    z <- connection
    cat("\n", file = z)
    vid <- vertex_attr(g, name = "name", index = vertex)    
    describedNodeNames <- c(describedNodeNames, vid)
    vclass <- vertex_attr(g, name = "className", index = vertex)    
    label <- vertex_attr(g, name = "label", index = vertex)    
    node.index <- getNodeIndexbyName(g, vid)
    if (isIndividualInstance(vid)) {
        cat("\t\"@id\": ", paste0("\"", vid, "\",\n"), file = z)    
    } else {
        cat("\t\"@id\": ", paste0("\"#", vid, "\",\n"), file = z)    
    }
    if (!is.null(vclass)) cat(paste0("\t\"@type\": \"", vclass, "\",\n"), file = z)
    if (!is.null(label)) cat(paste0("\t\"rdfs:label\": \"", label, "\""), file = z)
    # Propiedades planas
    pplist <- getPlainPropertyList(g, node.index)
    hasplainproperties <- FALSE
    if (length(pplist) > 0) {
        hasplainproperties <- TRUE    
        cat(",\n", file = z)
        for (j in 1:length(pplist)) {
            cat(paste0("\"", names(pplist)[j], "\":"), paste0("\"", pplist[[j]], "\""), file = z)
            if (j < length(pplist)) {
                cat(",\n", file = z)
            }   
        }
    }
    # Arcos que parten de este nodo: E(g)[from(getNodeIndexbyName(g, vid))]
    # full.list <- vertex_attr(g, index = vertices[i])
    arcs <- E(g)[.from(node.index)]
    if (length(arcs) > 0) { # El nodo tiene hijos
        cat("\n", file = z)
        classproperties <- edge_attr(g, name = "label", arcs)
        for (j in 1:length(unique(classproperties))) {
            currentProperty <- unique(classproperties)[j]
            # numero de hijos con esa propiedad
            nchildren <- sum(classproperties %in% currentProperty)
            ind.arc <- which(classproperties %in% currentProperty)
            cat(paste0(",\n\"", currentProperty, "\": "), file = z)
            if (nchildren == 1) {
                cat("{", file = z)
                child.node <- head_of(g, arcs[ind.arc])
                child.node.name <- vertex_attr(g, name = "name", index = child.node)
                # Si child.node ya está en describedNodeNames, skip serialize and point to identifier
                # <https://www.w3.org/TR/2014/REC-json-ld-20140116/#node-identifiers>
                if (!child.node.name %in% describedNodeNames) {
                    describedNodeNames <- serializeVertex(g, vertex = child.node, describedNodeNames, z)    
                } else {
                    if (isIndividualInstance(child.node.name)) {
                        cat("\t\"@id\": ", paste0("\"", child.node.name, "\"\n"), file = z)    
                    } else {
                        cat("\t\"@id\": ", paste0("\"#", child.node.name, "\"\n"), file = z)    
                    }
                } 
                cat("}", file = z)
            } else {
                cat("[", file = z)
                for (k in 1:nchildren) {
                    child.node <- head_of(g, arcs[ind.arc[k]])
                    child.node.name <- vertex_attr(g, name = "name", index = child.node)
                    cat("{", file = z)
                    # Si child.node ya está en describedNodeNames, skip serialize and point to identifier
                    # <https://www.w3.org/TR/2014/REC-json-ld-20140116/#node-identifiers>
                    if (!child.node.name %in% describedNodeNames) {
                        describedNodeNames <- serializeVertex(g, child.node, describedNodeNames, z)
                    } else {
                        if (isIndividualInstance(child.node.name)) {
                            cat("\t\"@id\": ", paste0("\"", child.node.name, "\"\n"), file = z)    
                        } else {
                            cat("\t\"@id\": ", paste0("\"#", child.node.name, "\"\n"), file = z)
                        }
                    }
                    cat("}", file = z)
                    if (k < nchildren) cat(",", file = z)
                }
                cat("]", file = z)
            }
        }
    }
    return(describedNodeNames)
}


#' @title Individual identification
#' @description Is the node representing and individual instance (\code{TRUE}), or a generic class entity (\code{FALSE})?
#' @return A Logical flag
#' @keywords internal
#' @author J Bedia

isIndividualInstance <- function(node.name) {
    grepl("^ds\\:|^veri\\:|^go\\:|^cal\\:|^ipcc\\::|^c6i\\:|^c6d\\:|^c6m\\:|^c6v\\:",
          node.name)
}
