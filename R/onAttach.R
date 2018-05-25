#' @importFrom utils packageDescription URLencode
#' @importFrom magrittr %>% 

.onAttach <- function(...) {
    pkgname <- "metaclipR"
    ver <- packageDescription(pkgname)$Version
    builddate <- packageDescription(pkgname)$Date
    mess <- paste(pkgname, " version ", ver, " (", builddate,") is loaded", sep = "")
    packageStartupMessage(mess)
    refURL <- paste0("http://metaclip-interpreter.predictia.es/individuals?vocab=http://www.metaclip.org/datasource&class=GCM") %>% URLencode() %>% url()
    on.exit(close(refURL))
    out <- tryCatch(suppressWarnings(readLines(refURL, warn = FALSE)), error = function(er) {
        er <- NULL
        return(er)
    })
    if (!is.null(out)) {
        packageStartupMessage("Succesfully opened the connection with remote ontology files")
    } else {
        packageStartupMessage("WARNING: Unable to open a connection with remote ontology files\nFull metadata details may be incomplete")
    }
}
# End

