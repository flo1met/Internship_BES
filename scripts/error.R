load(".../data/data_comp_10k.RData")
load(".../data/data_part_10k.RData")
library(tidyverse)
library(ggplot2)

# Calculate Error Measures
data_comp_filt <- data_comp %>% filter(n_sample %in% c(75, 150, 300, 1500)) %>%
  select(c(nsim, n_sample, d, c, PMP1u))
data_part_filt <- data_part %>% filter(study == 3 & n_sample != 5000) %>%
  mutate(n_sample = n_sample*3) %>%
  select(c(nsim, n_sample, d, c, BES_PMP1u))

data_error <- left_join(data_comp_filt, data_part_filt) %>%
  group_by(n_sample, d, c) %>%
  mutate(
    MAE = abs(PMP1u - BES_PMP1u), # mean average error
    MSE = (PMP1u - BES_PMP1u)^2, # mean squared error
    MAPE = abs((PMP1u - BES_PMP1u) / PMP1u) * 100 # mean average percentage error
  )

agg_errors <- data_error %>%
  summarise(MAE = mean(MAE),
            MSE = mean(MSE),
            MAPE = mean(MAPE))

# boxplot
data_error %>% filter(d == 0.2, c == 0.2) %>%
  ggplot(aes(x = as.factor(n_sample), y = MAPE)) +
  stat_summary(aes(x = as.factor(n_sample), y = MAPE), 
               fun = mean, 
               geom = "line", 
               group = 1, 
               linewidth = 0.2) +
  geom_boxplot(outlier.alpha = 0.3) +
  ylim(NA, 100) + # 25 outliers are removed
  labs(x = "Sample Size (total)", y = "Mean Average Error Percentage (%)") +
  theme_minimal() 

ggsave(".../plots/boxplot_error.pdf", width = 7, height = 7)

agg_errors %>% filter(d != 0) %>% ggplot() +
  geom_line(aes(x = as.factor(n_sample), y = MAPE, color = as.factor(d), group = 1)) +
  labs(x = "Effect Size", y = "Correlation") +
  scale_color_discrete(guide = "none") +
  theme_bw() +
  facet_grid(c~ d) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(".../plots/lineplot_error.pdf", width = 7, height = 7)

rm(agg_comp, agg_errors, agg_part, data_comp, data_comp_filt, data_error, data_part, data_part_filt)
