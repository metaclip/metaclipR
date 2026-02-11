##     igraph-helpers.R Helper functions to get information from igraph graphs
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


#' @title Get node indices
#' @description Get the node index positions from their names
#' @param graph \code{\link[igraph]{igraph}} graph 
#' @param names Character vector of names of the nodes whose index is queried
#' @family graph.helpers
#' @export
#' @importFrom igraph V make_graph
#' @author J Bedia
#' @return A vector of index positions corresponding to the input names  
#' @examples 
#' require(igraph)
#' graph <- make_graph(c("A", "B", "B", "C", "C", "D", "Pepe"), directed = FALSE)
#' getNodeIndexbyName(graph, "Pepe")
#' getNodeIndexbyName(graph, c("A","D","Pepe"))

getNodeIndexbyName <- function(graph, names) {
    out <- sapply(1:length(names), function(x) which(V(graph)$name == names[x]))
    names(out) <- names
    return(out)
}

#' @title Return all node attributes
#' @description Return a list of all (non-empty) node attributes
#' @param graph \code{\link[igraph]{igraph}} graph 
#' @param vertex.name Character string, with the target vertex name
#' @param vertex.index Alternatively, instead of \code{vertex.name}, an integer
#'  indicating the index position of the vertex in the graph. 
#' @family graph.helpers
#' @export
#' @seealso \code{\link{getNodeIndexbyName}}
#' @importFrom igraph vertex_attr
#' @author J Bedia
#' @return A list of (non-empty) attributes belonging to the vertex

getNodeAttributes <- function(graph, vertex.index = NULL, vertex.name = NULL) {
    if (is.null(vertex.index)) {
        if (is.null(vertex.name)) {
            stop("Either a valid 'vertex.index' or 'vertex.name' value is required", call. = FALSE)
        } else {
            vertex.index <- getNodeIndexbyName(graph, vertex.name)
        }
    } 
    lista <- vertex_attr(graph, index = vertex.index) 
    non.empty.id <- which(sapply(1:length(lista), function(x) !is.na(lista[[x]])))
    return(lista[non.empty.id])
}


#' @title Return most node attributes 
#' @description Return almost all node attributes, but the name, classname, etc...
#' @param g \code{\link[igraph]{igraph}} graph 
#' @param node.index An integer indicating the index position of the vertex in the graph. 
#' @family graph.helpers
#' @keywords internal
#' @importFrom igraph vertex_attr
#' @author J Bedia
#' @return A list of (non-empty) attributes belonging to the vertex (lists are vectorized)


getPlainPropertyList <- function(g, node.index) {
    attr.list <- vertex_attr(g, index = node.index)    
    # Remove already known attrs
    ind <- which(!(names(attr.list) %in% c("name", "label", "className", "description")))    
    attr.list.s <- attr.list[ind]
    plain.property.list <- attr.list.s[which(!is.na(unlist(attr.list.s)))]
    return(plain.property.list)
}

