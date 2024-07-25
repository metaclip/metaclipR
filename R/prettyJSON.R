#' @title Prettify or minify a METACLIP JSON
#' @description A wrapper of \code{\link[jsonlite]{prettify}} and \code{\link[jsonlite]{minify}} to modify METACLIP outputs for either readability or minimum size.
#' @param jsonfile Path to the JSON file
#' @param indent Positive integer. Number of spaces for indentation (ignored when \code{minify = TRUE})
#' @param minify Logical. If set to \code{TRUE}, removes all indentation and whitespaces to minimize the file size.
#' @return The input file is overwritten by its prettyfied/minified version
#' @author juaco
#' @seealso [graph2json()]
#' @importFrom jsonlite prettify minify

prettyJSON <- function(jsonfile, indent = 2, minify = FALSE) {
    stopifnot(is.logical(minify))
    myjson <- readChar(jsonfile, file.info(jsonfile)$size)
    prettyjson <- if (minify) {
        jsonlite::minify(txt = myjson)
    } else {
        jsonlite::prettify(txt = myjson, indent = indent)
    }
    # overwrite files with the new pretty layout
    fileConn <- file(jsonfile)
    writeLines(prettyjson, fileConn)
    close(fileConn)
}
