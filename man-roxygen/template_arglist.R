#' @section About argument lists:

#' Argument lists are key-value list in which the different arguments and their valus are passed 
#' to the command description. These were initially conceived as an adequate means of describing
#' command calls to interpreted languajes (R, python...) using both the Argument and Argument 
#' classes from the datasource vocabulary of METACLIP.
#' 
#' Alternatively, and in order to accomodate the description of the source code to other environments
#' and languajes (e.g. shell scripts, calls to CDOs etc.), the use of literal command calls 
#' is a more convenient choice. Literal command calls also allow for a more straightforward
#' reproducibility by just "copying and pasting" the code used to generate each step. Literal
#' command calls are encoded in METACLIP using the \emph{hadLiteralCommandCall} data property
#' from the datasource vocabulary, that is attached to the command description. 
#' To apply this second alternative, the \code{arg.list} argument is specified as a character string
#' containing the command call.
#' 
#' Note that default argument values are often omitted from comand calls. However, the default behaviour of 
#' commands may change depending on the specific software version, and it is therefore often advisable to 
#' explicitly include the default argument values for a more straightforward interpretation of the source code
#' by the user. 
