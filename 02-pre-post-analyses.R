# Analysis of pre- and post- workshop survey responses
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-03-26

library(dplyr)

#' Run t-test and return relevant statistics as a list
#' 
#' @param data two-column numeric vector (or data frame) for t-test.
#' @param xname,yname character names of columns to use for pre- and post-
#'   workshop survey answers
#' @param qname character name of question id column
#' @param ... additional arguments passed to \code{t.test}
#' @return list of statistics including summary stats and t-test results
#' @example 
#'   x <- rpois(n = 50, lambda = 5)
#'   y <- rpois(n = 50, lambda = 4)
#'   question_result(data = cbind(x, y), xname = "x", yname = "y", paired = TRUE)
question_result <- function(data, xname = "pre", yname = "post", 
                            qname = "question_id", ...) {
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
  mean_pre <- mean(x)
  mean_post <- mean(y)
  mean_delta <- mean_post - mean_pre
  
  # Run t-test
  t_test <- t.test(x = x,
                   y = y,
                   ...)
  
  # Package up results as list and return
  return(list(question_id = data[1, qname],
              mean_pre = mean_pre,
              mean_post = mean_post,
              mean_delta = mean_delta,
              t = t_test$statistic,
              df = t_test$parameter,
              p = t_test$p.value))
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

# Analyses (paired t-tests) on each question
results_list <- survey_data %>%
  group_by(Question_number) %>%
  group_map(~ question_result(data = .x, 
                              xname = "Pre",
                              yname = "Post",
                              qname = "Question_number",
                              paired = TRUE), 
            .keep = TRUE)

# Go ahead and turn the results into a tibble
results_table <- results_list %>%
  bind_rows()

# Calculate adjusted p values for multiple comparisons (Holm-Bonferroni); all 
# but five comparisons significant (three of the five were significant without 
# multiple comparison correction)
results_table$adj_p <- p.adjust(p = results_table$p,
                                method = "holm")

# Write this output of stats to file
write.csv(x = results_table,
          file = "output/pre-post-results.csv",
          row.names = FALSE)

################################################################################
# Want to know if starting (pre) values of knowledge were different from 
# starting values of confidence
question_info <- read.csv(file = "data/question-info.csv")

# Add the question info to the survey data
survey_data <- survey_data %>%
  left_join(question_info, by = c("Question_number" = "question_id"))

# We want a vector of knowledge pre scores and a vector of confidence pre 
# scores
knowledge_pre <- survey_data$Pre[survey_data$question_category %in% 
                                   c("Pedagogy knowledge", 
                                     "Skills development knowledge")]
confidence_pre <- survey_data$Pre[survey_data$question_category == 
                                    "Skills development confidence"]

pre_t_test <- t.test(x = knowledge_pre,
                     y = confidence_pre,
                     paired = FALSE,
                     var.equal = FALSE)
sink(file = "output/knowledge-confidence-t.txt")
pre_t_test
sink()
