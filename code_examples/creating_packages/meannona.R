#' Removes NA's and computes mean
#'
#top line is bold part of help file;  you should also name file the name of function for ease
#any line that contains #comma is read by knitR to create documentation
#look at mean (?mean) to see how this works
# here is example dummy function to make means always remove NA
#start with function name then empty line
#Description
#' This function allows you to calculate the mean value of a set while excluuding
#' NA's
# param is a list of your arguments; its the parameter followed by description
#' @param ... list for which you seeking a mean
# Value
#' @return returns the value for a list of objects, automatically removing any NA
#' values
#' @export
#  see also
#' @keywords basic functions
#  details
#' @details This function aids in creating the run file from a .csv file
# examples
#' @examples
#' meannona(c(0,1,2,NA))
#' @export
# put function code here
meannona = function(...){
  mean(..., na.rm=T)
}
