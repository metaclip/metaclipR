##     getClass-helpers.R Helper functions to infer Class values from the verification ontology
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

#' @title Get Forecast Representation
#' @description Infer the Class inheriting from Abstract Class ForecastRepresentation from the value of verifun argument
#' @param verifun A character string with the verifun argument value
#' @return A character string of the ForecastRepresentation Class
#' @source \url{https://docs.google.com/presentation/d/1-H-d5X2kdA6-0nqnCd5yLs_V1DeTzuTGw2g3bLhtj7I/present#slide=id.g23b883c69b_0_194}
#' @keywords internal
#' @family getClass.helpers
#' @author J Bedia

getForecastRepresentation <- function(verifun) {
    fr <- switch(verifun,
                 "EnsMe" = "DeterministicContinuous",
                 "EnsMae" = "DeterministicContinuous",
                 "EnsMaess" = "DeterministicContinuous",
                 "EnsMse" = "DeterministicContinuous",
                 "EnsMsess" = "DeterministicContinuous",
                 "EnsRmse" = "DeterministicContinuous",
                 "EnsRmsess" = "DeterministicContinuous",
                 "cor.test" = "DeterministicContinuous",
                 "EnsCorr" = "DeterministicContinuous",
                 "cor" = "DeterministicContinuous",
                 "EnsRoca" = "BinaryProbability",
                 "EnsRocss" = "BinaryProbability",
                 "EnsBs" = "BinaryProbability",
                 "FairBrier" = "BinaryProbability",
                 "FairBrierSs" = "BinaryProbability",
                 "EnsSprErr" = "ContinuousProbability",
                 "FairSprErr" = "ContinuousProbability",
                 "EnsRps" = "MulticategoryProbability",
                 "EnsRpss" = "MulticategoryProbability",
                 "EnsCrps" = "Ensemble",
                 "FairCrps" = "Ensemble",
                 "Ens2AFC" = "Ensemble")
    return(paste0("veri:", fr))
}


#' @title Get QualityAspect class value
#' @description Infer the Class inheriting from Abstract Class QualityAspect from the value of verifun argument
#' @param verifun A character string with the verifun argument value
#' @return A character string of the QualityAspect Class
#' @source \url{https://docs.google.com/presentation/d/1-H-d5X2kdA6-0nqnCd5yLs_V1DeTzuTGw2g3bLhtj7I/present#slide=id.g23b883c69b_0_194}
#' @keywords internal
#' @family getClass.helpers
#' @author J Bedia

getQualityAspect <- function(verifun) {
    qa <- switch(verifun,
                 "EnsMe" = "Bias",
                 "EnsMae" = "Accuracy",
                 "EnsMaess" = "Accuracy",
                 "EnsMse" = "Accuracy",
                 "EnsMsess" = "Accuracy",
                 "EnsRmse" = "Accuracy",
                 "EnsRmsess" = "Accuracy",
                 "EnsCorr" = "Association",
                 "cor" = "Association",
                 "cor.test" = "Association",
                 "EnsRoca" = "Discrimination",
                 "EnsRocss" = "Discrimination",
                 "EnsBs" = "Accuracy",
                 "FairBrier" = "Accuracy",
                 "FairBrierSs" = "Accuracy",
                 "EnsSprErr" = "Reliability",
                 "FairSprErr" = "Reliability",
                 "EnsRps" = "Accuracy",
                 "EnsRpss" = "Accuracy",
                 "EnsCrps" = "Accuracy",
                 "FairCrps" = "Accuracy",
                 "Ens2AFC" = "Discrimination")
    return(paste0("veri:", qa))
}

#' @title Get Verification class node label
#' @description Give a label to the specific measure used for the Verification node, as inferred from the value of verifun argument
#' @param verifun A character string with the verifun argument value
#' @return A character string of the verification measure label
#' @keywords internal
#' @family getLabel.helpers
#' @author J Bedia

getVerificationLabel <- function(verifun) {
    switch(verifun,
           "EnsMe" = "BIAS",
           "EnsMae" = "MAE",
           "EnsMaess" = "MAE-SS",
           "EnsMse" = "MSE",
           "EnsMsess" = "MSE-SS",
           "EnsRmse" = "RMSE",
           "EnsRmsess" = "RMSE-SS",
           "EnsCorr" = "CORR",
           "cor.test" = "CORR",
           "cor" = "CORR",
           "EnsRoca" = "ROCA",
           "EnsRocss" = "ROCSS",
           "EnsBs" = "BS",
           "FairBrier" = "F-BS",
           "FairBrierSs" = "F-BS-SS",
           "EnsSprErr" = "SPRERR",
           "FairSprErr" = "F-SPRERR",
           "EnsRps" = "RPS",
           "EnsRpss" = "RPSS",
           "EnsCrps" = "CRPS",
           "FairCrps" = "F-CRPS",
           "Ens2AFC" = "2AFC")
}

