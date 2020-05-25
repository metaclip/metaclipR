#' @title ERA-Interim example dataset
#' @format A climate4R grid
#' @name ERAIN_tas_iberia
#' @description ERA-Interim reanalysis data in a 2-degree regular grid centred on the Iberian Peninsula. Data correspond to summer
#' (JJA) temperature and are annually aggregated, encompassing the period 1981-2010. 
#' @docType data
#' @source Santander MetGroup User Data Gateway \url{http://www.meteo.unican.es/udg-tap}
# @examples \donttest{
# require(transformeR)
# require(visualizeR)
# data("ERAIN_tas_iberia")
# spatialPlot(climatology(ERAIN_tas_iberia),
#             backdrop.theme = "countries", 
#             scales = list(draw = TRUE),
#             main = getVarNames(ERAIN_tas_iberia, type = "long")
# )
# range(getRefDates(ERAIN_tas_iberia))            
# getSeason(ERAIN_tas_iberia) 
# getTimeResolution(ERAIN_tas_iberia)   
# }
NULL

#' @title System4 Predictions example dataset
#' @format A climate4R grid
#' @name S4_tas_iberia
#' @description System-4 seasonal predictions in a 2-degree regular grid centred on the Iberian Peninsula. Data correspond to summer
#' (JJA) temperature predictions (15 members) issued in May (lead-month 1) and are annually aggregated, encompassing the period 1981-2010. 
#' @docType data
#' @source Santander MetGroup User Data Gateway \url{http://www.meteo.unican.es/udg-tap}
#' @references Cofiño, A.S. \emph{et al.} 2017. The ECOMS User Data Gateway: Towards seasonal forecast data provision and research reproducibility in the era of Climate Services. Climate Services. doi:10.1016/j.cliser.2017.07.001
# @examples \donttest{
# require(transformeR)
# require(visualizeR)
# data("S4_tas_iberia")
# spatialPlot(climatology(S4_tas_iberia),
#             backdrop.theme = "coastline", 
#             scales = list(draw = TRUE),
#             main = getVarNames(S4_tas_iberia, type = "long")
# )
# range(getRefDates(S4_tas_iberia))            
# getSeason(S4_tas_iberia) 
# getTimeResolution(S4_tas_iberia)  
# } 
NULL
