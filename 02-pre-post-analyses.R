# Analysis of pre- and post- workshop survey responses
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-03-26

library(dplyr)

#' Run test and return relevant statistics as a list
#' 
#' @param data two-column numeric vector (or data frame) for test.
#' @param xname,yname character names of columns to use for pre- and post-
#'   workshop survey answers
#' @param qname character name of question id column
#' @param test character indicating test to run; "t" for a t-test, "T" for a 
#' Wilcoxon signed rank
#' @param ... additional arguments passed to \code{t.test} or \code{wilcox.test}
#' @return list of statistics including summary stats and test results
#' @example 
#'   x <- rpois(n = 50, lambda = 5)
#'   y <- rpois(n = 50, lambda = 4)
#'   question_result(data = cbind(x, y), xname = "x", yname = "y", paired = TRUE)
question_result <- function(data, xname = "post", yname = "pre", 
                            qname = "question_id", test = c("t", "T"), ...) {
  # Yeah, tibbles won't work in this function
  data <- as.data.frame(data)
  
  # Drop rows that are missing values in x and/or y
  data <- data[!is.na(data[, xname]), ]
  data <- data[!is.na(data[, yname]), ]

  # Do some QA/QC
  # Make sure data for only one question is sent in
  if (!is.null(qname)) {
    if (qname %in% colnames(data)) {
      if (length(unique(data[, qname])) > 1) {
        warning("Data for more than one question sent to question_result()")
      }
    }
  }
  
  # Pull out vectors of interest
  x <- data[, xname]
  y <- data[, yname]
  
  # Calculate mean pre, post, and difference
  mean_post <- mean(x)
  mean_pre <- mean(y)
  mean_delta <- mean_post - mean_pre
  
  if (test == "t") {
    # Run t-test
    h_test <- t.test(x = x,
                     y = y,
                     ...)
  } else if (test == "T") {
    # Run Wilcoxon
    h_test <- wilcox.test(x = x,
                          y = y, 
                          ...)
  } else {
    stop("Unrecognized value for test (", test, "), should be 't' or 'T'.")
  }
  
  # Package up results as list and return
  results_list <- list(question_id = data[1, qname],
              mean_pre = mean_pre,
              mean_post = mean_post,
              mean_delta = mean_delta,
              df = h_test$parameter,
              p = h_test$p.value)
  # Only need confidence intervals for t-test (not Wilcoxon), also we use a 
  # different name for our statistic column based on test (t-test: t, 
  # Wilcoxon: T)
  if (test == "t") {
    results_list["t"] <- h_test$statistic
    results_list["lwr"] <- h_test$conf.int[1]
    results_list["upr"] <- h_test$conf.int[2]
  } else {
    results_list["T"] <- h_test$statistic
  }
  return(results_list)
}

# Reality check #1
# x <- rpois(n = 50, lambda = 5)
# y <- rpois(n = 50, lambda = 4)
# question_result(data = cbind(x, y), xname = "x", yname = "y", paired = TRUE)

# Reality check #2, with group_map
# questions <- data.frame(x = x, y = y, id = rep(c("A", "B")))
# questions %>%
#   group_by(id) %>%
#   group_map(~ question_result(data = .x, xname = "x", yname = "y", paired = TRUE))

# Read in data
survey_data <- read.csv(file = "data/survey-data-processed.csv")

# Clean up data. For now, we don't need term, participant, or text
survey_data <- survey_data %>%
  select(-c(Term, Participant.Code, Question_text))

# Before writing results to file, will add question text
question_info <- read.csv(file = "data/question-info.csv")

####################################
#  Normality tests (Shapiro-Wilk)  #
####################################
# For t-tests, we will want to test the assumption that our difference in 
# scores is normally distributed

# Calculate the difference in Post and Pre scores
delta_data <- survey_data %>%
  mutate(Delta = Post - Pre) %>%
  select(Question_number, Delta)

#' Run Shapiro-Wilk test for normality
#' 
#' @param data Data frame including numeric column of differences between Post- 
#' and Pre-workshop survey answers
#' @param dname Character name of column in \code{data} where differences are 
#' stored
#' @param qname Character name of column in \code{data} where the question 
#' identifier is stored
sw_test <- function(data, dname = "Delta", qname = "Question_number") {
  data <- as.data.frame(data)
  deltas <- data[, dname]
  results <- shapiro.test(x = deltas)
  return(list(p.value = results$p.value,
              W = results$statistic,
              Question_number = data[1, qname]))
}

# Run Shapiro-Wilk test on each question separately, then bind results into a 
# single tibble
sw_results <- delta_data %>%
  group_by(Question_number) %>%
  group_map(~ sw_test(data = .x),
            .keep = TRUE) %>%
  bind_rows()

# See if any of the questions do not violate normality assumption
any(sw_results$p.value >= 0.05)
# FALSE

# All fail the normality test, so present non-parametric results instead of 
# t-test

# Note: t-test results retained for posterity, but should probably not be used 
# for interpretation. Wilcoxon signed-rank test results (which follow the t-test 
# results) should be used instead.

####################
#      t-tests     #
####################
# Analyses (paired t-tests) on each question
t_results <- survey_data %>%
  group_by(Question_number) %>%
  group_map(~ question_result(data = .x, 
                              xname = "Post",
                              yname = "Pre",
                              qname = "Question_number",
                              test = "t",
                              paired = TRUE), 
            .keep = TRUE)

# Go ahead and turn the results into a tibble
t_table <- t_results %>%
  bind_rows()

# Calculate adjusted p values for multiple comparisons (Holm-Bonferroni); all 
# but five comparisons significant (three of the five were significant without 
# multiple comparison correction)
t_table$adj_p <- p.adjust(p = t_table$p,
                          method = "holm")

# Add the question info to the results df
t_table <- t_table %>%
  left_join(question_info, by = join_by(question_id))

# Write this output of stats to file
write.csv(x = t_table,
          file = "output/pre-post-results-t.csv",
          row.names = FALSE)

##################
# Wilcoxon tests #
##################
# Analyses (Wilcoxon signed-rank) on each question
T_list <- survey_data %>%
  group_by(Question_number) %>%
  group_map(~ question_result(data = .x, 
                              xname = "Post",
                              yname = "Pre",
                              qname = "Question_number",
                              test = "T",
                              paired = TRUE,
                              exact = FALSE), 
            .keep = TRUE)

# Go ahead and turn the results into a tibble
T_table <- T_list %>%
  bind_rows()

# Calculate adjusted p values for multiple comparisons (Holm-Bonferroni)
T_table$adj_p <- p.adjust(p = T_table$p,
                                method = "holm")

# Add the question info to the results df
T_table <- T_table %>%
  left_join(question_info, by = join_by(question_id))

# Write this output of stats to file
write.csv(x = T_table,
          file = "output/pre-post-results-T.csv",
          row.names = FALSE)

################################################################################
# Want to know if starting (pre) values of skills development confidence were 
# different from starting values of data science confidence
question_info <- read.csv(file = "data/question-info.csv")

# Add the question info to the survey data
survey_data <- survey_data %>%
  left_join(question_info, by = c("Question_number" = "question_id"))

# We want a vector of knowledge pre scores and a vector of confidence pre 
# scores
skills_pre <- survey_data$Pre[survey_data$question_category == "Skills development confidence"]
data_science_pre <- survey_data$Pre[survey_data$question_category == "Data science confidence"]

# t-test
pre_t_test <- t.test(x = skills_pre,
                     y = data_science_pre,
                     paired = FALSE,
                     var.equal = FALSE)
sink(file = "output/confidence-t.txt")
pre_t_test
sink()

# Mann Whitney
pre_U_test <- wilcox.test(x = skills_pre,
                          y = data_science_pre,
                          paired = FALSE)
sink(file = "output/confidence-U.txt")
pre_U_test
sink()
