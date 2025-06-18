# Analyze stipend data
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-06-17

library(dplyr)

# These data are a bit funky, as we have a categorical predictor variable 
# (career status) and an ordinal response variable (No/Maybe/Yes). Use a non-
# parametric test: Kruskal-Wallis rank sum

stipend_data <- read.csv(file = "data/stipend-data-processed.csv")

# A couple of rows with missing stipend data, drop those (remove 2 rows)
stipend_data <- stipend_data[!is.na(stipend_data$Stipend_influence), ]

# The "Other" career status may be difficult to interpret, so one could drop 
# those, too (would remove 4 rows)
# stipend_data <- stipend_data[stipend_data$Status != "Other", ]

# Turn two variables of interest into factors
stipend_data$Status <- factor(x = stipend_data$Status,
                              levels = c("Staff", "Career Track", "Tenure Track", "Other"))

stipend_data$Stipend_influence <- factor(x = stipend_data$Stipend_influence,
                                         levels = c("No", "Maybe", "Yes"))

# table(stipend_data$Status, stipend_data$Stipend_influence)

# Do K-W test
stipend_kw <- kruskal.test(Stipend_influence ~ Status, data = stipend_data)
stipend_kw
# Kruskal-Wallis rank sum test
# 
# data:  Stipend_influence by Status
# Kruskal-Wallis chi-squared = 4.4091, df = 3, p-value = 0.2205

# If there was a significant difference, we could possibly look at pairwise 
# comparisons via pairwise.wilcox.test()

# Nothing super-exciting, but still want to report % of No/Maybe/Yes
stipend_summary <- stipend_data %>%
  group_by(Stipend_influence) %>%
  summarize(Count = n())
stipend_summary$Percentage <- stipend_summary$Count/nrow(stipend_data)
# Stipend_influence   Count Percentage
#  <fct>              <int>      <dbl>
# No                      7      0.233
# Maybe                  10      0.333
# Yes                    13      0.433

# For the manuscript, we can create a contingency table
stipend_table <- table(stipend_data$Stipend_influence, stipend_data$Status)

# Add the column for count
stipend_table <- cbind(stipend_table, rowSums(stipend_table))
# Update the name of that new column
colnames(stipend_table)[ncol(stipend_table)] <- "Total"

# Add the column for percentage
stipend_table <- cbind(stipend_table, stipend_table[, "Total"]/sum(stipend_table[, "Total"]))
# Update column name, then turn into a percentage
colnames(stipend_table)[ncol(stipend_table)] <- "Percent"
stipend_table[, "Percent"] <- round(stipend_table[, "Percent"] * 100, 2)

write.csv(x = stipend_table, file = "output/stipend-table.csv")
