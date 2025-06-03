# Visualization and tabulation of pre/post analysis results
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-04-10

# Plots and table are based on the output of 02-pre-post-analyses.R script
# output/pre-post-results.csv

library(dplyr)
library(ggplot2)
library(stringr) # For wrapping long axis labels

# Will have four separate plots (facets), for four types of questions
#   1. Knowledge of pedagogical best practices, especially in regards to skills 
#      development
#   2. Knowledge of how to apply best practices for skills development
#   3. Confidence in using best practices for skills development
#   4. Confidence in developing data science materials

results <- read.csv(file = "output/pre-post-results.csv")

# Add indicator of whether adjusted p is significant or not
results <- results %>%
  mutate(sig = adj_p < 0.05)

# Need to level things so they appear in the order we want on the plot
# Leveling categories so knowledge panels (2) show up a top and middle and 
# confidence panel shows up at the bottom
results <- results %>%
  mutate(question_category = factor(x = question_category,
                                    levels = c("Pedagogy knowledge",
                                               "Skills development knowledge",
                                               "Skills development confidence",
                                               "Data science confidence")))

# Leveling questions so low-numbered questions (e.g. Q09) are at top, instead 
# of bottom
results <- results %>%
  mutate(question_id = factor(x = question_id,
                              levels = rev(unique(question_id))))

# UPDATE: Single figure is now too crowded. Do to separate figures (below)
# Will want to indicate which questions had significant differences between pre
# and post score. For now, using shade (gray is N.S., black is significant)
# Experimented with linetype (dashed N.S.), but sometimes had no line because 
# of small difference between pre- and post-workshop scores
pre_post_plot <- ggplot(data = results, mapping = aes(y = question_id)) +
  geom_errorbar(mapping = aes(xmin = mean_pre, 
                              xmax = mean_post,
                              color = sig), width = 0) +
  # scale_linetype_manual(values = c(2, 1)) + # dashed for N.S. (sig == FALSE)
  geom_point(mapping = aes(x = mean_pre,
                           color = sig), 
             shape = 21, size = 3, fill = "#FFFFFF") +
  geom_point(mapping = aes(x = mean_post,
                           color = sig), 
             shape = 19, size = 3) +
  scale_color_manual(values = c("#B0B0B0", "#000000")) +
  scale_y_discrete(breaks = results$question_id,
                     # labels = results$question_text) +
                   labels = stringr::str_wrap(results$question_text, width = 50)) +
  scale_x_continuous(limits = c(1, 5)) +
  facet_grid(question_category ~ ., 
             scales = "free_y",
             space = "free_y") + # For smarter use of vertical space
  labs(x = "Mean response",
       y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        legend.position = "none",
        strip.background = element_rect(fill = "#F0F0F0"),
        strip.text = element_text(size = 10))

ggsave(filename = "output/pre-post-visualization.png",
       plot = pre_post_plot,
       height = 8,
       width = 6.5,
       units = "in")

plot_pre_post <- function(results_to_plot) {
  pre_post_plot <- ggplot(data = results_to_plot, mapping = aes(y = question_id)) +
    geom_errorbar(mapping = aes(xmin = mean_pre, 
                                xmax = mean_post,
                                color = sig), width = 0) +
    # scale_linetype_manual(values = c(2, 1)) + # dashed for N.S. (sig == FALSE)
    geom_point(mapping = aes(x = mean_pre,
                             color = sig), 
               shape = 21, size = 3, fill = "#FFFFFF") +
    geom_point(mapping = aes(x = mean_post,
                             color = sig), 
               shape = 19, size = 3) +
    scale_color_manual(values = c("#B0B0B0", "#000000")) +
    scale_y_discrete(breaks = results_to_plot$question_id,
                     labels = stringr::str_wrap(results_to_plot$question_text, 
                                                width = 50)) +
    scale_x_continuous(limits = c(1, 5)) +
    facet_grid(question_category ~ ., 
               scales = "free_y",
               space = "free_y") + # For smarter use of vertical space
    labs(x = "Mean response",
         y = NULL) +
    theme_bw() +
    theme(axis.text = element_text(size = 10),
          legend.position = "none",
          strip.background = element_rect(fill = "#F0F0F0"),
          strip.text = element_text(size = 10))
}

knowledge_pre_post <- results %>%
  filter(question_category %in% c("Pedagogy knowledge",
                                  "Skills development knowledge")) %>%
  plot_pre_post()
ggsave(filename = "output/pre-post-viz-knowledge.png",
       plot = knowledge_pre_post,
       height = 5,
       width = 6.5,
       units = "in")

confidence_pre_post <- results %>% 
  filter(question_category %in% c("Skills development confidence",
                                  "Data science confidence")) %>%
  plot_pre_post()
ggsave(filename = "output/pre-post-viz-confidence.png",
       plot = confidence_pre_post,
       height = 5,
       width = 6.5,
       units = "in")

################################################################################
# A table of statistical results with question type, question text, t, df, adj p
results_table <- results %>%
  select(question_category, question_text, t, df, adj_p)

# Let's go ahead and round t
results_table <- results_table %>%
  mutate(t = round(t, digits = 2))

# Now, we want a column for p, but not with all the significant digits. 
# Anything less than 0.001 should be replaced with "< 0.001"
results_table <- results_table %>%
  mutate(p = if_else(adj_p < 0.001, 
                     true = "<0.001",
                     false = as.character(round(adj_p, digits = 3))))

# Since we are *only* reporting adjusted p-values, drop the "adj_p" column
results_table <- results_table %>%
  select(-adj_p)

# And now order things so they show up in same order as the figure
results_table <- results_table %>%
  arrange(question_category, question_text)

# And write to csv
write.csv(file = "output/pre-post-table.csv",
          row.names = FALSE,
          x = results_table)
