# Transform raw data to format appropriate for analyses
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-04-03

library(tidyr)
library(dplyr)

# Read in data for session 1
raw_1 <- read.csv(file = "data/survey-data-raw-pd1.csv")

# Start by filtering out any second or third year responses
raw_1 <- raw_1 %>%
  group_by(Participant.Code) %>%
  arrange(Term) %>%
  slice_head(n = 1) %>%
  ungroup()

# Do the same for session 2
raw_2 <- read.csv(file = "data/survey-data-raw-pd2.csv")
raw_2 <- raw_2 %>%
  group_by(Participant.Code) %>%
  arrange(Term) %>%
  slice_head(n = 1) %>%
  ungroup()

# For both sessions, we can drop the session number from the Term column, this 
# will make joins easier
raw_1 <- raw_1 %>%
  mutate(Term = substr(x = Term, start = 1, stop = 4))
raw_2 <- raw_2 %>%
  mutate(Term = substr(x = Term, start = 1, stop = 4))


# Join these two data together by person and Term (which is now effectively
# Year). Note some participants (3 total) did session 1 and session 2 in 
# different years. Will result in weirdness until we pivot to long
raw <- raw_1 %>%
  full_join(raw_2, by = join_by(Participant.Code, Term))

# Transform to long
raw_long <- raw %>%
  pivot_longer(cols = -c(Term, Participant.Code),
               names_to = "Question",
               values_to = "Response")

# The multi-year delinquents results in a bunch of NAs to remove
raw_long <- raw_long %>%
  filter(!is.na(Response))

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
# and one column each for pre & post responses. 
# Move em out, head em up, RAW WIDE!
raw_wide <- raw_long %>%
  pivot_wider(names_from = Question_point,
              values_from = Response)

# And since we need both pre/post anwsers for our purposes, remove rows with 
# missing values in Pre or Post
raw_wide <- raw_wide %>%
  filter(!is.na(Pre)) %>%
  filter(!is.na(Post))

# Write to file
write.csv(file = "data/survey-data-processed.csv",
          row.names = FALSE,
          x = raw_wide)
