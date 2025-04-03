# Transform raw data to format appropriate for analyses
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-04-03

library(tidyr)
library(dplyr)

# Read in data
raw <- read.csv(file = "data/survey-data-raw.csv")

# Transform to long
raw_long <- raw %>%
  pivot_longer(cols = -c(Term, Participant.Code),
               names_to = "Question",
               values_to = "Response")

# For easier sorting later, let's pre-append a zero before question 9
raw_long <- raw_long %>%
  mutate(Question = gsub(x = Question,
                         pattern = "Q9P",
                         replacement = "Q09P"))

# Parse question text (former column names) into number and text
# Need to start by adding a delimiter between question number and the Pre/Post
# designation. Underscore is already taken (part of sub-question delimitation, 
# i.e. 10_1, 10_2), so use ZZ. Yeah. Cool.
raw_long <- raw_long %>%
  mutate(Question = gsub(pattern = "Pre",
                         replacement = "ZZPre",
                         x = Question)) %>%
  mutate(Question = gsub(pattern = "Post",
                         replacement = "ZZPost",
                         x = Question))

# Now use that delimiter to split the question number off into another column
raw_long <- raw_long %>%
  separate_wider_delim(cols = Question,
                       delim = "ZZ",
                       names = c("Question_number", "Question_text"))

# Next pull out pre or post from the question text
raw_long <- raw_long %>%
  mutate(Question_point = if_else(substr(x = Question_text,
                                         start = 1,
                                         stop = 3) == "Pre",
                                  true = "Pre",
                                  false = "Post"))

# Delete the "Pre../Post.." from start of question text. regex #@$*
raw_long <- raw_long %>%
  mutate(Question_text = gsub(pattern = "Pre\\.\\.|Post\\.\\.",
                              replacement = "",
                              x = Question_text))

# Transform to wider, with one row per participant, one column for the question
# and one column each for pre & post responses
raw_wide <- raw_long %>%
  pivot_wider(names_from = Question_point,
              values_from = Response)

# Write to file
write.csv(file = "data/survey-data-processed.csv",
          row.names = FALSE,
          x = raw_wide)
