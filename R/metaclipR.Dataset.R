##     metaclipR.Dataset Define the initial node of a METACLIP graph with data source metadata
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

#' @title Directed metadata graph construction for Data Sources
#' @description Build a directed metadata graph describing a data source. This is usually the initial 
#' step to build METACLIP graphs
#' 
#' @param Dataset.name Name (label) of the Dataset. There is a number of already known Datasets, pertaining to the User Data Gateway 
#' Public datasets of the climate4R Framework (see References). Type \code{showUDGDatasources()} for details.
#'  If the argument corresponds to any of these named instances, the associated provenance information will be automatically recorded,
#'  and all other arguments can be omitted.
#' @param Dataset.subclass A character string indicating the Dataset subclass. The only (and disjoint) possible values 
#' are \code{"MultiDecadalSimulation"}, \code{"ObservationalDataset"}, \code{"Reanalysis"}, \code{"SeasonalHindcast"},
#'  \code{"SeasonalOperationalForecast"} and \code{"ShortRangeForecast"}.
#' @param DataProvider A Character string indicating the data provider of the Dataset (e.g. \code{"UDG"}, 
#'  for datasets accessed through the Santander MetGroup User Data Gateway, see References).
#'   NOTE: Any ds:ModellingCenter can be also be ds:DataProvider. For a list of known (instantiable) data providers see
#'    \code{knownClassIndividuals("DataProvider")} and \code{knownClassIndividuals("ModellingCenter")}. 
#' @param ModellingCenter Optional character vector, e.g. \code{"ECMWF"}, or \code{c("SMHI","KNMI")}, in case there are two or more modelling centers involved.
#'  See \code{knownClassIndividuals("ModellingCenter")} for a list of known (instantiable) modelling centers.
#' @param DataProvider.URL Optional: The URL from which the data was accessed. 
#' @param Project Optional: Character string indicating the Project leading to the generatrion of the Dataset (e.g. \code{"CORDEX"}).
#'  See \code{knownClassIndividuals("Project")} for a list of known (instantiable) modelling centers.
#' @param RCM Optional. Character string (length one). Simulation RCM model prducing the dataset (e.g. \code{"RACMO22E"}). 
#'  See \code{knownClassIndividuals("RCM")} for a list of known (instantiable) RCMs.
#' @param GCM Optional. Character string (length one). GCM producing the dataset
#'  See \code{knownClassIndividuals("GCM")} for a list of known (instantiable) GCMs (e.g. \code{"EC-EARTH"}).
#' @param Run Optional. When relevant, a character string (length one) indicating the model run (e.g. \code{"r1i1p1"})
#' 
#' @details This function takes as reference the semantics defined in the Data Source and Transformation ontology
#' defined in the Metaclip Framework (\url{http://www.metaclip.org/}).
#'  Many of the classes represented have individuals defined that can be directly instantiated instead of the Class itself. This has the advantage
#'  of providing further annotations relevant for provenance description (e.g. references, relevant URLs, comments etc.).
#'   This is automatically handled by the function when there is perfect match between the argument value and the known individual name.
#'    To ensure consistency, the use of the helper function \code{\link{knownClassIndividuals}} is recommended.
#'   
#' 
#' @family transformation
#' @export
#' @importFrom igraph make_empty_graph add_vertices add_edges 
#' @author D. San Martín, J. Bedia
#' @examples 
#' 
#' # showUDGDatasources() will display all the known datasets from the User Data Gateway.
#' # With just indicating the dataset name (and the provider, which is not assumed to be UDG,
#' # so dataset metadata can be reused if accessed through other providers), the rest of metadata
#' # will be appennded. Further annotations are included inside the nodes (e.g. access URL,
#' # model run etc. Not shown by the i-graph plot display)
#' 
#' # A GCM 
#' dataset <- "CMIP5_CNRM-CERFACS-CNRM-CM5_historical"
#' gcm.dataset <- metaclipR.Dataset(Dataset.name = dataset, DataProvider = "UDG")
#' plot(gcm.dataset$graph)                              
#' 
#' # An RCM (note the hadDrivingGCM property)
#' dataset <- "EUROCORDEX11_MOHC-HadGEM2-ES_r1i1p1_RCA4_v1_rcp45"
#' rcm.dataset <- metaclipR.Dataset(Dataset.name = dataset,
#'                                  DataProvider = "UDG")
#' plot(rcm.dataset$graph)                              
#' 
#' # When dealing with known Individuals, the node is kept fix whenever it is necessary to refer to it:
#' metadata <- metaclipR.Dataset(Dataset.name = "E-OBS_v14_0.25regular",
#'                               DataProvider = "KNMI")
#' plot(metadata$graph)


metaclipR.Dataset <- function(Dataset.name = NULL, 
                              Dataset.subclass = NULL,
                              DataProvider = NULL,
                              ModellingCenter = NULL,
                              DataProvider.URL = NULL,
                              Project = NULL, 
                              RCM = NULL, 
                              GCM = NULL,
                              Run = NULL) {
    if (!is.character(Dataset.name) | length(Dataset.name) != 1) stop("Incorrect 'Dataset.name' argument value")
    # Load reference datasources table -------------
    ref <- showUDGDatasources()
    metadata <- NULL
    isKnownDatasource <- ifelse(Dataset.name %in% ref$name, TRUE, FALSE)
    if (isKnownDatasource) {
        message("The Dataset is in the internal reference table: all datasource metadata will be automatically appended")
        metadata <- ref[grep(Dataset.name, ref$name), ]
        Dataset.subclass <- metadata$DatasetSubclass
        ModellingCenter <- metadata$ModellingCenter %>% strsplit(split = ";") %>% unlist()
        Project <- metadata$Project
        if (DataProvider == "UDG") DataProvider.URL <- metadata$url
        RCM <- metadata$RCM
        GCM <- metadata$GCM
        Run <- metadata$Run
    }
    ref <- NULL
    # Dataset Subclass definition
    if (!is.character(Dataset.subclass)) stop("A valid 'Dataset.subclass' value is required", call. = FALSE)
    Dataset.subclass <- match.arg(Dataset.subclass, choices = c("MultiDecadalSimulation",
                                                                "ObservationalDataset",
                                                                "Reanalysis",
                                                                "SeasonalHindcast",
                                                                "SeasonalOperationalForecast",
                                                                "ShortRangeForecast"))
    graph <- make_empty_graph(directed = TRUE)
    # Dataset node ------------------
    Dataset.nodename <- paste0("Dataset.", randomName())
    graph <- my_add_vertices(graph,
                             nv = 1,
                             name = Dataset.nodename,
                             label = Dataset.name,
                             className = paste0("ds:", Dataset.subclass))
    # DataProvider node (OPTIONAL) -------------
    # List of instantiable knownDataProviders
    if (is.character(DataProvider)) {
        if (length(DataProvider) > 1) stop("Only one DataProvider per Dataset is allowed", call. = FALSE)
        isKnownDataProvider <- ifelse(DataProvider %in% suppressMessages((knownClassIndividuals("DataProvider"))) | 
                                          DataProvider %in% suppressMessages((knownClassIndividuals("ModellingCenter"))), TRUE, FALSE)
        DataProvider.nodename <- ifelse(isKnownDataProvider, paste0("ds:", DataProvider), paste0("DataProvider.", randomName())) 
        graph <- my_add_vertices(graph,
                                 nv = 1,
                                 name = DataProvider.nodename,
                                 label = DataProvider,
                                 className = "ds:DataProvider",
                                 attr = list("ds:hasMainURL" = DataProvider.URL))
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, Dataset.nodename),
                             getNodeIndexbyName(graph, DataProvider.nodename)),
                           label = "ds:hadDataProvider")
    }
    # ModellingCenter OPTIONAL --------------------------------
    if (is.character(ModellingCenter)) {
        for (i in 1:length(ModellingCenter)) {
            isKnownModellingCenter <- ifelse(ModellingCenter[i] %in% suppressMessages(knownClassIndividuals("ModellingCenter")), TRUE, FALSE)
            ModellingCenter.nodename <- ifelse(isKnownModellingCenter, paste0("ds:", ModellingCenter[i]), paste0("ModellingCenter.", randomName())) 
            graph <- my_add_vertices(graph,
                                     nv = 1,
                                     name = ModellingCenter.nodename,
                                     label = ModellingCenter[i],
                                     className = "ds:ModellingCenter")
            graph <- add_edges(graph, 
                               c(getNodeIndexbyName(graph, Dataset.nodename),
                                 getNodeIndexbyName(graph, ModellingCenter.nodename)),
                               label = "ds:hadModellingCenter")
        }
    }
    # Project OPTIONAL ------------------------------------
    if (!is.null(Project)) {
        isKnownProject <- ifelse(Project %in% suppressMessages(knownClassIndividuals("Project")), TRUE, FALSE)
        Project.nodename <- ifelse(isKnownProject, paste0("ds:", Project), paste0("Project.", randomName())) 
        graph <- my_add_vertices(graph,
                                 nv = 1,
                                 name = Project.nodename,
                                 label = Project,
                                 className = "ds:Project")
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, Dataset.nodename),
                             getNodeIndexbyName(graph, Project.nodename)),
                           label = "ds:hadProject")
    }
    # Simulation model --------------------------------------
    rcmdata <- FALSE
    if (isTRUE(suppressWarnings(is.na(RCM)))) RCM <- NULL
    if (!is.null(RCM)) {
        rcmdata <- TRUE
        isKnownRCM <- ifelse(RCM %in% knownClassIndividuals("RCM"), TRUE, FALSE)
        RCM.nodename <- ifelse(isKnownRCM, paste0("ds:", RCM), paste0("RCM.", randomName()))
        graph <- my_add_vertices(graph,
                                 nv = 1,
                                 name = RCM.nodename,
                                 label = RCM,
                                 className = "ds:RCM",
                                 attr = list("ds:hasRun" = Run))
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, Dataset.nodename),
                             getNodeIndexbyName(graph, RCM.nodename)),
                           label = "ds:hadSimulationModel")
    }
    if (isTRUE(suppressWarnings(is.na(GCM)))) GCM <- NULL
    if (!is.null(GCM)) {
        isKnownGCM <- ifelse(GCM %in% knownClassIndividuals("GCM"), TRUE, FALSE)
        GCM.nodename <- ifelse(isKnownGCM, paste0("ds:", GCM), paste0("GCM.", randomName()))
        if (rcmdata) Run <- NA
        graph <- my_add_vertices(graph,
                                 nv = 1,
                                 name = GCM.nodename,
                                 label = GCM,
                                 className = "ds:GCM",
                                 attr = list("ds:hasRun" = Run))
        graph <- if (rcmdata) {
            add_edges(graph, 
                      c(getNodeIndexbyName(graph, RCM.nodename),
                        getNodeIndexbyName(graph, GCM.nodename)),
                      label = "ds:hadDrivingGCM")
        } else {
            add_edges(graph, 
                      c(getNodeIndexbyName(graph, Dataset.nodename),
                        getNodeIndexbyName(graph, GCM.nodename)),
                      label = "ds:hadSimulationModel")
        }
        
    }
    return(list("graph" = graph, "parentnodename" = Dataset.nodename))
}
