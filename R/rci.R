# Create a function that is used to compute the Reliable Change Index (RCI)
# The function takes in 3 inputs: (a) a data frame, (b) name of the pretest
# column (in quotes), and (c) name of the posttest column (in quotes). The
# function creates a new column for the RCI, merges it with the input data
# frame, and returns it back to the user. The function produces NAs for cases
# that has at least 1 NA value in either the pretest or the posttest column
rci <- function(df, pretest, posttest, method = "JT") {

  # Test for missing dataset argument
  if (missing(df)) {
    stop("Dataset is missing")
  }

  # Test for missing pre-test scores argument
  if (missing(pretest)) {
    stop("Pre-test scores are missing")
  }

  # Test for missing post-test scores argument
  if (missing(posttest)) {
    stop("Post-test scores are missing")
  }

  # Test whether the df object is a data frame. If not, coerce it into a
  # data frame
  if (!is.data.frame(df)) {
    df <- as.data.frame(df)
  }

  # If method is "JT" (Jacobson & Truax, 1991)
  if (method == "JT") {

    # Compute the numerator. Subtract pre-test scores from post-test scores
    numerator <- ifelse(is.na(df[, pretest]) | is.na(df[, posttest]),
                        NA, df[, posttest] - df[, pretest])

    # Compute the test-retest reliability score. Use pairwise deletion
    test_retest <- cor(df[, pretest], df[, posttest],
                       use = "pairwise.complete.obs")

    # Compute the denominator from the standard deviation from the pre-test
    # scores multiplied by the square root of 1 - test-retest reliability
    denominator <- sd(df[, pretest], na.rm = TRUE) * sqrt(1 - test_retest)

    # Compute the RCI and name the newly created variable by appending
    # "_RCI" to the post-test column name
    df[, paste0(posttest, "_RCI")] <- round(numerator/denominator, 2)

    # Return the original data frame together with the newly created
    # RCI variable
    return(df)
  }

  # If method is "CM" (Christensen & Mendoza, 1986)
  else if (method == "CM") {

    # Compute the numerator. Subtract pre-test scores from post-test scores
    numerator <- ifelse(is.na(df[, pretest]) | is.na(df[, posttest]),
                        NA, df[, posttest] - df[, pretest])

    # Compute the test-retest reliability score. Use pairwise deletion
    test_retest <- cor(df[, pretest], df[, posttest],
                       use = "pairwise.complete.obs")

    # Compute the denominator from the square root of the sum of the variance
    # of the pre-test scores and the variance of the post-test scores minus
    # 2 multiplied with the standard deviation of the pre-test scores
    # multiplied with the standard deviation of the post-test scores
    # multiplied by the test-retest reliability
    denominator <- sqrt(var(df[, pretest], na.rm = TRUE) +
                          var(df[, posttest], na.rm = TRUE) - (2 *
                          sd(df[, pretest], na.rm = TRUE) *
                          sd(df[, posttest], na.rm = TRUE) * test_retest))

    # Compute the RCI and name the newly created variable by appending
    # "_RCI" to the post-test column name
    df[, paste0(posttest, "_RCI")] <- round(numerator/denominator, 2)

    # Return the original data frame together with the newly created
    # RCI variable
    return(df)
  }

  # If method is "I" (Iverson et al., 2003)
  else if (method == "I") {

    # Compute the numerator. Subtract pre-test scores from post-test scores
    numerator <- ifelse(is.na(df[, pretest]) | is.na(df[, posttest]),
                        NA, df[, posttest] - df[, pretest])

    # Compute the test-retest reliability score. Use pairwise deletion
    test_retest <- cor(df[, pretest], df[, posttest],
                       use = "pairwise.complete.obs")

    # Compute the SEM for the pre-test scores
    sem1 <- sd(df[, pretest], na.rm = TRUE) * sqrt(1 - test_retest)

    # Compute the SEM for the post-test scores
    sem2 <- sd(df[, posttest], na.rm = TRUE) * sqrt(1 - test_retest)

    # Compute the denominator from the square root of the sum of the squared
    # SEMs for each testing occasion
    denominator <- sqrt((sem1 ^ 2) + (sem2 ^ 2))

    # Compute the RCI and name the newly created variable by appending
    # "_RCI" to the post-test column name
    df[, paste0(posttest, "_RCI")] <- round(numerator/denominator, 2)

    # Return the original data frame together with the newly created
    # RCI variable
    return(df)
  }

  # If method is "L" (Lewis et al., 2007)
  else if (method == "L") {

    # Compute the numerator. Subtract pre-test scores from post-test scores
    numerator <- ifelse(is.na(df[, pretest]) | is.na(df[, posttest]),
                        NA, df[, posttest] - df[, pretest])




  }

  # If method is "M" (McSweeney et al., 1993)
  else if (method == "M") {

    # Compute the numerator. Subtract pre-test scores from post-test scores
    numerator <- ifelse(is.na(df[, pretest]) | is.na(df[, posttest]),
                        NA, df[, posttest] - df[, pretest])

    # Compute the test-retest reliability score. Use pairwise deletion
    test_retest <- cor(df[, pretest], df[, posttest],
                       use = "pairwise.complete.obs")

    # Compute the denominator from the standard deviation of the least-squares
    # regression residuals
    denominator <- sd(df[, posttest], na.rm = TRUE) * sqrt(1 - test_retest ^ 2)

    # Compute the RCI and name the newly created variable by appending
    # "_RCI" to the post-test column name
    df[, paste0(posttest, "_RCI")] <- round(numerator/denominator, 2)

    # Return the original data frame together with the newly created
    # RCI variable
    return(df)
  }
}
