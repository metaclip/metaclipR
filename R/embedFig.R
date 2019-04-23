##     embedFig.R A function to embed compressed JSON metadata into bitmap files
##
##     Copyright (C) 2019 Predictia (http://www.predictia.es)
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

#' @title Generate a figure bitmap file with embedded metadata
#' @description The function calls a plotting function, and writes to a user-defined bitmap file, including
#' the full metadata description provided by the user. This information is a compressed JSON representation.
#' @param plot.fun Character. Name of the plotting function
#' @template template_arglistParam
#' @template template_arglist
#' @param full.metadata An igraph-class containing the RDF graph with the full metadata structure.
#' @param format Format of the bitmap file. Currently only the \code{"png"} option is available
#' @param filename The name of the output file.
#' @param ... further optional arguments passed to \code{\link{png}} (width, height etc.)
#' @return A temporary file of the specified format
#' @author J. Bedia
#' @importFrom grDevices png dev.off
#' @importFrom  png writePNG readPNG
#' @importFrom magrittr %>% 
#' @family graphical.outputs
#' @details The function performs the following steps:
#' \enumerate{
#' \item Write the output figure in a temporary file
#' \item Serialize the RDF graph into json and write it into a temporary file
#' \item Read the temporary json file and compress (gzip)
#' \item Read the temporary image file, and re-write into the destination file (as given in \code{filename}),
#' appending the compressed metadata
#' }
#' @export

embedFig <- function(plot.fun, arg.list, full.metadata, format = "png", filename, ...) {
    opt.list <- list(...)
    if (class(full.metadata) != "igraph") stop("'full.metadata' is not an igraph-class object")
    # opt.list <- list("width" = 950, "height" = 900, "res" = 150)
    ext <- paste0(".", format)
    # Create a temporary file
    tempfile.out <- tempfile(fileext = ext)
    opt.list[["filename"]] <- tempfile.out
    do.call(format, opt.list)
    # png(filename = tempfile.out, width = 950, height = 900, res = 150)
    do.call(plot.fun, arg.list) %>% print()
    dev.off()
    tempfile.json <- tempfile(fileext = ".json")
    # Serialize RDF graph to JSON and write to temp file
    graph2json(graph = full.metadata, output.file = tempfile.json)
    txt <- scan(tempfile.json,
                what = character(), strip.white = TRUE,
                quote = "'", blank.lines.skip = TRUE)
    # compress metadata to gzip
    metadata <- paste(txt, collapse = "") %>% charToRaw() %>% memCompress() %>% as.character() %>% paste(collapse = "")
    metadata <- c(metaclip = metadata)
    # Read temporary plot file
    img <- readPNG(source = tempfile.out)
    # Embed compressed metadata
    writePNG(image = img,
             text = metadata,
             target = filename)
    message("[", Sys.time(), "] ", filename ," was sucessfully generated")
}



#' @title Embed JSON-LD
#' @description Embeds a JSON file into a png as METACLIP metadata
#' @param png.in Full path to a target png file
#' @param json.file File path to JSON-LD 
#' @param png.out Output png file path
#' @return A png with embeded METACLIP provenance
#' @importFrom png readPNG writePNG
#' @importFrom magrittr %>% 
#' @export
#' @family graphical.outputs
#' @author J Bedia

embedJSON <- function(png.in, json.file, png.out) {
    txt <- scan(json.file,
                what = character(), strip.white = TRUE,
                quote = "'", blank.lines.skip = TRUE)
    metadata <- paste(txt, collapse = " ") %>% charToRaw() %>% memCompress() %>% as.character() %>% paste(collapse = "")
    metadata <- c(metaclip = metadata)
    img <- png::readPNG(source = png.in)
    # Embed compressed metadata
    png::writePNG(image = img,
                  text = metadata,
                  target = png.out)
}
