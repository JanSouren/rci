rci_class <- function(df, pretest, posttest, method = "JT") {

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

  # Coerce df argument input into a data frame
  df <- as.data.frame(df)

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
    # "RCI" to the post-test column name
    df[, paste0(posttest, "RCI")] <- round(numerator/denominator, 2)

    # Assign each case into one of three classes (Positive, No Change,
    # Negative) based on RCI cut-off scores
    df[, paste0(posttest, "_RCI")] <- as.factor(ifelse(df[, paste0(posttest, "RCI")]
                                                             >= 1.96, "Positive",
                                                             ifelse(df[, paste0(posttest, "RCI")]
                                                                    <= -1.96, "Negative", "No Change")))

    # Delete the numeric RCI
    df[, paste0(posttest, "RCI")] <- NULL

    # Return the original data frame together with the newly created
    # RCI variable
    return(df)
  }
}
