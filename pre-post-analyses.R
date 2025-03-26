# Analysis of pre- and post- workshop survey responses
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-03-26

# Read in data

# Do data cleaning
#   Clean up column names?
#   Extract unique question names

# Analyses (paired t-tests) on each question

# Table to hold results
# question | mean pre | mean post | mean delta | t | p | adjusted p
t_results <- data.frame(question = questions,
                        mean_pre = NA_real_,
                        mean_post = NA_real_,
                        mean_delta = NA_real_,
                        t = NA_real_,
                        p = NA_real_,
                        adj_p = NA_real_)

# Iterate over each unique question name
#   Pull out the two relevant columns
#   Drop rows missing one or both values
#   Calculate means of pre, post, and delta & add to table
#   Run t-test
#   Extract t, p, effect size (mean difference?)
#   Add results to table

# Calculate adjusted p values for multiple comparisons; 
# consider Holm-Bonferroni approach https://en.wikipedia.org/wiki/Holm%E2%80%93Bonferroni_method