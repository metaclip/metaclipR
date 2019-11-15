##     metaclip-helpers.R Helper internals for metaclipR
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


#' @title Generate a random character string
#' @description Generates a random character string (lowercase) of a specified length. 
#' Used to avoid duplicated node names in igraph
#' @param len length of the character string
#' @export
#' @keywords internal
#' @author J Bedia
#' @return A string with the specified number of characters
#' @importFrom magrittr %>% extract
#' @examples 
#' randomName()
#' randomName(10)

randomName <- function(len = 6) {
    ind <- sample(1:length(letters)) %>% extract(1:len) 
    letters[ind] %>% paste(collapse = "")
}


#' @title Table of User Data Gateway PUBLIC datasets
#' @description Show the internal table with known Datasets and relevant metadata
#' @export
#' @author J Bedia
#' @return A \code{data.frame}
#' @export
#' @importFrom utils read.csv
#' @examples showUDGDatasources()[c(2,35,60,81),]

showUDGDatasources <- function() {
    read.csv(file.path(find.package("metaclipR"), "datasource.csv"),
             stringsAsFactors = FALSE, na.strings = "")
}


#' @title List all individuals from a class
#' @description List defined individuals pertaining to a specific class from a METACLIP vocabulary
#' @param classname The parent class from which the individual instances are queried
#' @param vocabulary The target vocabulary name. Possible values are \code{"datasource"} (the default),
#'  \code{"calibration"}, \code{"verification"} and \code{"graphical_output"}.
#'  @param source.vocab Vocabulary the queried individuals belong to. For instance, the ClimateIndex class
#'  belong to the datasource vocabulary, but other individuals of this class may belong 
#'  to other vocabularies (e.g. ipcc).
#' @importFrom utils URLencode
#' @importFrom magrittr %>% 
#' @details The function will check the existing individuals in the latest stable target
#' ontology release
#' @return A character vector of individuals for that class
#' @note The function won't work if there is no internet connection, or any other connection problem prevents
#'  read access to the ontology file.
#' @export
#' @author J Bedia, D. San-Martín
#' @family ontology.helpers
#' @examples 
#' knownClassIndividuals("ModellingCenter")
#' knownClassIndividuals("ETCCDI")
#' # In case a class is misspelled or it has no individuals, 
#' # an empty vector is returned, with a warning:
#' knownClassIndividuals("ETCDDI")
#' knownClassIndividuals("Dataset")

knownClassIndividuals <- function(classname, vocabulary = "datasource", source.vocab = NULL) {
    vocabulary <- match.arg(vocabulary, choices = c("datasource",
                                                    "calibration",
                                                    "verification",
                                                    "graphical_output",
                                                    "ipcc_terms"))
    if (is.null(source.vocab)) source.vocab <- vocabulary
    refURL <- paste0("http://www.metaclip.org/individuals?vocab=", vocabulary, "&sourceVocab=", source.vocab, "&class=")
    message("Reading remote ", vocabulary, " ontology file ...")
    destURL <- paste0(refURL, classname) %>% URLencode() %>% url() 
    on.exit(close(destURL))
    print(destURL)
    out <- tryCatch(suppressWarnings(readLines(destURL, warn = FALSE)), error = function(er) {
        er <- NULL
        return(er)
    })
    if (!is.null(out)) {
        a <- gsub("\"|\\[|]", "", out) %>% strsplit(split = ",") %>% unlist() 
        if (length(a) == 0) warning("No individuals found:\nEither the class does not exist or there are no individuals associated to it")
        return(a)
    } else {
        message("Unreachable remote vocabulary\nLikely reason: unavailable internet connection")
    }
}


#' @title Identify the class belonging of an individual
#' @description Identifies the class belonging of an individual. Superclasses are ignored
#' @param individual.name Name of the individual
#' @param vocabulary The target vocabulary name. Possible values are \code{"datasource"} (the default),
#'  \code{"calibration"}, \code{"verification"} and \code{"graphical_output"}.
#' @importFrom utils URLencode
#' @importFrom magrittr %>% extract2
#' @details The function will check the existing individuals in the latest stable target 
#' ontology release
#' @return A character string with the most direct class belonging (superclasses are not indicated)
#' @note The function won't work if there is no internet connection, or any other connection problem prevents
#'  read access to the ontology file.
#' @export
#' @author J Bedia, D. San-Martín
#' @family ontology.helpers
#' @examples
#' getIndividualClass("UDG") # The default vocabulary is datasource (can be omitted)
#' getIndividualClass("EQM", vocabulary = "calibration")

getIndividualClass <- function(individual.name, vocabulary = "datasource") {
    vocabulary <- match.arg(vocabulary, choices = c("datasource",
                                                    "calibration",
                                                    "verification",
                                                    "graphical_output"))
    refURL <- paste0("http://www.metaclip.org/individual?vocab=", vocabulary, "&id=")
    message("Reading remote ", vocabulary, " ontology file ...")
    destURL <- paste0(refURL, individual.name) %>% URLencode() %>% url() 
    on.exit(close(destURL))
    out <- tryCatch(suppressWarnings(readLines(destURL, warn = FALSE)), error = function(er) {
        er <- NULL
        return(er)
    })
    if (!is.null(out)) {
        aux <- gsub("\"|\\[|]", "", out)
        if (aux == "") {
            a <- NULL
        } else {
            a <-  aux %>% strsplit(split = ",") %>% unlist() %>% extract2(1) %>% gsub(pattern = "http://www.metaclip.org/.*\\.owl#", replacement = "")
            if (length(a) == 0) warning("No class found:\nEither the individual does not exist or there are no associated classes to it")
        }
        return(a)
    } else {
        message("Unreachable remote vocabulary\nLikely reason: unavailable internet connection")
    }
}



#' @title Internal helper for vertex addition
#' @description A wrapper of \code{igraph::add_vertex} to avoid adding vertex refering to individuals already defined in the graph
#' @param graph i-graph class graph on which the node is added (might be an empty graph if it is the initial Dataset node)
#' @param name vertex name
#' @param nv Integer. Number of vertices added. Default (and only sensible value) to 1.
#' @param label A \dQuote{rdfs:label} data property, useful for displaying the graph in visualization APIs
#' @param className Character. Name of the class that the node belongs to.
#'  Requires prefix inclusion (e.g. \code{className = "ds:DatasetSubset"})
#' @importFrom igraph add_vertices vertex_attr
#' @keywords internal
#' @export
#' @author J Bedia

my_add_vertices <- function(graph,
                            name = NULL,
                            nv = 1,
                            label = NULL,
                            className = NULL,
                            attr = NULL) {
    if (class(graph) != "igraph") stop("The input graph has not a valid format")
    if (is.null(name)) stop("The 'name' attribute is required", call. = FALSE)
    if (!name %in% vertex_attr(graph, name = "name")) {
        if (is.null(className)) stop("The 'className' attribute is required", call. = FALSE)
        if (is.null(label)) stop("The 'label' attribute is required", call. = FALSE)
        if (is.null(attr)) {
            graph <- add_vertices(graph,
                                  nv = nv,
                                  name = name,
                                  label = label,
                                  className = className)    
        } else {
            graph <- add_vertices(graph,
                                  nv = nv,
                                  name = name,
                                  label = label,
                                  className = className,
                                  attr = attr)
        }
    }
    return(graph)
}

#' @title Union of graphs
#' @description Performs union of multiple graphs, internally controlling for possible common nodes
#' @param ... igraph-class graphs to be joined
#' @return Returns (invisibly) a new igraph-class graph that is a union and/or disjoint union of the input graphs
#' @importFrom igraph vertex_attr disjoint_union union
#' @importFrom magrittr %>% 
#' @author J Bedia
#' @keywords internal
#' @export
#' @examples 
#' require(igraph)
#' # A disjoint union
#' g1 <- make_graph(c("1", "2", "2", "3", "3", "4", "4", "5", "4", "6"), directed = TRUE)
#' plot(g1)
#' g2 <- make_graph(c("7", "8", "8", "9", "9", "10", "9", "11"), directed = TRUE)
#' plot(g2)
#' g <- my_union_graph(g1, g2)
#' plot(g)
#' # A union of multiple graphs (one disjoint, two connected)
#' g3 <- make_graph(c("11", "10", "11", "12", "12", "13"), directed = TRUE)
#' plot(g3)
#' g <- my_union_graph(g1, g2, g3)
#' plot(g)
#' # A complete union of graphs
#' g4 <- make_graph(c("11", "1", "11", "10", "11", "12", "12", "13", "3", "12"), directed = TRUE)
#' g <- my_union_graph(g1, g2, g3, g4)
#' plot(g)

my_union_graph <- function(...) {
    graph.list <- list(...)
    graph <- graph.list[[1]]
    for (i in 2:length(graph.list)) {
        int <- intersect(vertex_attr(graph, name = "name"), vertex_attr(graph.list[[i]], name = "name")) %>% length()
        if (int == 0L) {
            graph <- disjoint_union(graph, graph.list[[i]])    
        } else {
            g <- igraph::union(graph, graph.list[[i]])
            graph <- my_union(g, graph, graph.list[[i]])
        }
    }
    invisible(graph)
}


#' @title Attribute-preserving igraph union
#' @description Transforms the resulting output from igraph::union to adequately recover the original attributes.
#' Apart from vertex attributes, the "label" attribute from edges, used to define object properties, is fixed.
#' @param g The union graph
#' @param g1 First input graph
#' @param g2 Second input graph
#' @return Invisible returns a new graph with fixed attributes
#' @author J. Bedia
#' @keywords internal
#' @export
#' @importFrom magrittr %>% 
#' @importFrom igraph vertex_attr vertex_attr_names delete_vertex_attr get.edgelist edge_attr delete_edges add_edges set_vertex_attr
#' @importFrom stats na.exclude

my_union <- function(g, g1, g2) {
    # Vertex attr adjustment
    ind.vertex <- match(vertex_attr(g, "name"), c(vertex_attr(g1, "name"), vertex_attr(g2, "name")))
    duplicated.names <- intersect(vertex_attr_names(g1), vertex_attr_names(g2))
    duplicated.names <- duplicated.names[-match("name", duplicated.names)]
    if (length(duplicated.names) > 0) {
        for (i in 1:length(duplicated.names)) {
            all.vnames <- c(vertex_attr(g1, duplicated.names[i]), vertex_attr(g2, duplicated.names[i]))
            g <- set_vertex_attr(graph = g, name = duplicated.names[i], value = all.vnames[ind.vertex])
            rmv <- grep(paste0(duplicated.names[i], "_[:1-2:]$"), vertex_attr_names(g), value = TRUE)
            g <- delete_vertex_attr(g, rmv[1])
            g <- delete_vertex_attr(g, rmv[2])
        }
        # Edge label adjustment
        if (!is.null(edge_attr(g, "label_1"))) {
            edgelist <- get.edgelist(g)
            edgelabels <- edge_attr(g, "label_2")
            ind <- which(is.na(edgelabels))
            if (length(ind) > 0) {
                edgelabels[ind] <- edge_attr(g, "label_1")[ind]                
            }
            g <- delete_edges(g, E(g))
            for (i in 1:nrow(edgelist)) {
                g <- add_edges(g,
                               c(getNodeIndexbyName(g, edgelist[i,1]),
                                 getNodeIndexbyName(g, edgelist[i,2])),
                               label = edgelabels[i])
            }
        }
    }
    invisible(g)
}



#' @importFrom utils read.csv
#' @keywords internal

pkgVersionCheck <- function(pkg, version) {
    pkgv <- package_version(version)
    ref <- file.path(find.package(package = "metaclipR"), "pkg_versions.csv") %>% read.csv(stringsAsFactors = FALSE)
    if (!pkg %in% ref$package) {
        message("NOTE: The specified package is not a known ds:Package. Package version info will be annotated but not checked")
    } else {
        v <- ref[match(pkg, ref$package), 2:3]
        if (pkgv >= package_version(v[1]) && pkgv <= package_version(v[2])) {
            message("Valid package version")
        } else {
            warning("The package version provided is not guaranteed to be properly handled by metaclipR\nType 'knownPackageVersions()' for details.")
        }
    }
}



#' @title List of valid package versions for metaclipR
#' @description Print a table with the valid version range of known packages. For the listed packages and version ranges, 
#' metaclipR fuctions are guaranteed to correctly map arguments/functions to the corresponding METACLIP classes/properties.
#' @importFrom utils read.csv
#' @importFrom magrittr %>% 
#' @export

knownPackageVersions <- function() find.package(package = "metaclipR") %>% file.path("pkg_versions.csv") %>% read.csv(stringsAsFactors = FALSE) %>% print()

#' @title Set a node name
#' @description Set a node name after checking whether it is an individual instance or not
#' @export
#' @param node.name Proposed node name. This ca be an individual instance
#' @param node.class Class of the node
#' @param vocabulary Vocabulary defining the \code{node.class}. Default to \code{"datasource"}.
#' @return A character string to be set as the \code{"name"} attribute by \code{\link{my_add_vertices}}

setNodeName <- function(node.name, node.class, vocabulary = "datasource") {
    suppressWarnings(
        ifelse(node.name %in% suppressMessages(knownClassIndividuals(node.class, vocabulary = vocabulary)),
               paste0("ds:", node.name),
               paste0(node.name, ".", randomName()))
    )
}



