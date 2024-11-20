load("../data/data_comp_10k.RData")
load("../data/data_part_10k.RData")
library(tidyverse)
library(ggplot2)


# Lineplot
agg_part_filt <- agg_part %>% filter(study == 3 & n_sample != 5000)
agg_part_filt$n_sample <- agg_part_filt$n_sample*3

agg_comp_filt <- agg_comp %>% filter(n_sample %in% c(75, 150, 300, 1500))


aggRes <- left_join(agg_part_filt, agg_comp_filt, by = c("d", "c", "n_sample"))

aggRes %>% ggplot() +
  geom_line(aes(x = as.factor(n_sample), y = PMP1u.y, linetype = "Complete", color = as.factor(d), group = 1)) +
  geom_line(aes(x = as.factor(n_sample), y = BES_PMP1u, linetype = "Partial", color = as.factor(d), group = 1)) +
  scale_linetype_manual(name = "Hypothesis", values = c("Complete" = "solid", "Partial" = "longdash")) +
  geom_hline(yintercept = 8/(8+1), color = "black", linetype = "dotted", alpha = 0.5) +
  labs(x = "Effect Size", y = "Correlation") +
  scale_color_discrete(guide = "none") +
  theme_bw() +
  facet_grid(c~ d) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../plots/lineplot.pdf", width = 7, height = 7)

# Boxplots
data_comp %>% 
  filter(study == 3, n_sample %in% c(75, 150, 300, 500), c == 0.2, d == 0.2) %>%
  ggplot(aes(x = as.factor(n_sample), y = PMP1u)) +
  stat_summary(aes(x = as.factor(n_sample), y = PMP1u), 
               fun = mean, 
               geom = "line", 
               group = 1, 
               linewidth = 0.2) +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_hline(yintercept = 24/(24+1), color = "black", linetype = "dashed", alpha = 0.8) +
  ylim(0, 1) + 
  labs(x = "Sample Size", y = "Posterior Model Probabilities") +
  theme_minimal()

ggsave("../plots/boxplot_comp.pdf", width = 7, height = 7)

data_part %>% 
  filter(study == 3, n_sample != 5000, c == 0.2, d == 0.2) %>%
  ggplot(aes(x = as.factor(n_sample), y = BES_PMP1u)) +
  stat_summary(aes(x = as.factor(n_sample), y = BES_PMP1u), 
               fun = mean, 
               geom = "line", 
               group = 1, 
               linewidth = 0.2) +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_hline(yintercept = 8/(8+1), color = "black", linetype = "dashed", alpha = 0.8) +
  ylim(0, 1) + 
  labs(x = "Sample Size (per Study)", y = "Posterior Model Probabilities") +
  theme_minimal()

ggsave("../plots/boxplot_part.pdf", width = 7, height = 7)

# Violinplots
data_comp %>% 
  filter(study == 3, n_sample %in% c(75, 150, 300, 500), c == 0.2, d == 0.2) %>%
  ggplot(aes(x = as.factor(n_sample), y = PMP1u)) +
  stat_summary(aes(x = as.factor(n_sample), y = PMP1u), 
               fun = mean, 
               geom = "line", 
               group = 1, 
               linewidth = 0.2) +
  geom_violin() +
  geom_hline(yintercept = 24/(24+1), color = "black", linetype = "dashed", alpha = 0.8) +
  ylim(0, 1) + 
  labs(x = "Sample Size", y = "Posterior Model Probabilities") +
  theme_minimal()

ggsave("../plots/violin_comp.pdf", width = 7, height = 7)

data_part %>% 
  filter(study == 3, n_sample != 5000, c == 0.2, d == 0.2) %>%
  ggplot(aes(x = as.factor(n_sample), y = BES_PMP1u)) +
  stat_summary(aes(x = as.factor(n_sample), y = BES_PMP1u), 
               fun = mean, 
               geom = "line", 
               group = 1, 
               linewidth = 0.2) +
  geom_violin() +
  geom_hline(yintercept = 8/(8+1), color = "black", linetype = "dashed", alpha = 0.8) +
  ylim(0, 1) + 
  labs(x = "Sample Size (per Study)", y = "Posterior Model Probabilities") +
  theme_minimal()

ggsave("../plots/violin_part.pdf", width = 7, height = 7)

rm(agg_comp, agg_comp_filt, agg_part, agg_part_filt, aggRes, data_comp, data_part)
