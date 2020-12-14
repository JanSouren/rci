#' Check variable for numeric data type
#'
#' This function checks a given variable for a numeric data type
#'
#' @param df Data frame
#' @param var Variable
#'
#' @return Message (if variable is not of a numeric data type)
#'
#' @export
#'

# Create a function to check whether the data type of a given variable
# is numeric. The function takes in 2 arguments: (a) a data frame, and
# (b) name of the variable (in quotes). If the given variable is not of
# a numeric data type, the function stops and prints out a message to
# the user
num_check <- function(df, var) {

  # Check the data type of var. If it is not numeric, stop
  if (is.numeric(df[, var]) == F) {
    stop(paste0(var, " is not numeric"))
  }
}
