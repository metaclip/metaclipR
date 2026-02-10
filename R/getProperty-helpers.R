##     getProperty-helpers.R Helper functions to infer plain property values 
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

#' @title Get the hasProbabilityValue property
#' @description Infer the hasProbabilityValue property from the arguments passed to \code{veriApply}
#' @param ForecastRepresentation Character string designating the ForecastRepresentation Class
#' @template template_arglistParam
#' @template template_arglist
#' @return Either \code{NULL} (if the property does not apply), or a list of attributes to be appended to the node.
#' @family getProperty.helpers
#' @keywords internal
#' @author J Bedia

getHasProbabilityThreshold <- function(ForecastRepresentation, arg.list) {
    attr.list <- list("veri:hasProbabilityThreshold" = NULL)
    if (grepl("Probability", ForecastRepresentation)) {
        if (!is.null(arg.list$prob)) {
            attr.list <- lapply(arg.list$prob, "[")
            names(attr.list) <- paste("veri:hasProbabilityThreshold", 1:length(attr.list), sep = ".")
        }
    }
    return(attr.list)
}

#' @title Get the hasAbsoluteThreshold property
#' @description Infer the hasAbsoluteThreshold property from the arguments passed to \code{veriApply}
#' @param ForecastRepresentation Character string designating the ForecastRepresentation Class
#' @template template_arglistParam
#' @template template_arglist
#' @keywords internal
#' @return Either \code{NULL} (if the property does not apply), or a list of attributes to be appended to the node.
#' @family getProperty.helpers
#' @author J Bedia

getHasAbsoluteThreshold <- function(ForecastRepresentation, arg.list) {
    attr.list <- list("veri:hasAbsoluteThreshold" = NULL)
    if (grepl("Probability", ForecastRepresentation)) {
        if (!is.null(arg.list$threshold)) {
            attr.list <- lapply(arg.list$threshold, "[")
            names(attr.list) <- paste("veri:hasAbsoluteThreshold", 1:length(attr.list), sep = ".")
        }
    }
    return(attr.list)
}

#' @title Get ISO-8601 time interval definition
#' @description Maps the time resolution output of \code{\link[transformeR]{getTimeResolution}} into a ISO-8601 duration specification
#' @param grid A climate4R grid
#' @return A character string with the time resolution specification compliant with ISO-8601 (or \code{"unknown"})
#' @importFrom transformeR getTimeResolution
#' @keywords internal
#' @references \url{https://en.wikipedia.org/wiki/ISO_8601#Durations}
#' @family getProperty.helpers
#' @author J Bedia

getHasTimeStep <- function(grid) {
    time.res <- suppressWarnings(getTimeResolution(grid))
    switch(time.res,
           "1h" = "P1H",
           "3h" = "P3H",
           "6h" = "P6H",
           "12h" = "P12H",
           "DD" = "P1D",
           "MM" = "P1M",
           "YY" = "P1Y",
           "unknown" = "unknown")
}


