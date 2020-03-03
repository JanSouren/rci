# Create a function that is used to compute the Reliable Change Index (RCI)
# The function takes in 3 inputs: (a) a data frame, (b) name of the pretest
# column (in quotes), and (c) name of the posttest column (in quotes). The
# function creates a new column for the RCI, merges it with the input data
# frame, and returns it back to the user. The function produces NAs for cases
# that has at least 1 NA value in either the pretest or the posttest column
RCI <- function(df, pretest, posttest) {

  # Test for missing dataset argument
  if(missing(df)) {
    stop("Dataset is missing")
  }

  # Test for missing pre-test scores argument
  if(missing(pretest)) {
    stop("Pre-test scores are missing")
  }

  # Test for missing post-test scores argument
  if(missing(posttest)) {
    stop("Post-test scores are missing")
  }

  numerator <- ifelse(is.na(df[, pretest]) | is.na(df[, posttest]),
                      NA, df[, posttest] - df[, pretest])

  test_retest <- cor(df[, pretest], df[, posttest],
                     use = "pairwise.complete.obs")

  denominator <- sd(df[, pretest], na.rm = TRUE) * sqrt(1 - test_retest)

  df[, paste0(posttest, "_RCI")] <- round(numerator/denominator, 2)
  return(df)
}
