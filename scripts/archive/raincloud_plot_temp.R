data_comp %>% 
  filter(study == 3, n_sample %in% c(75, 150, 300, 1500), c == 0.2, d == 0.2) %>%
  ggplot(aes(y = factor(n_sample), x = PMP1u, fill = factor(n_sample))) +
  
  # add half-violin from {ggdist} package
  #stat_halfeye(
  # adjust bandwidth
  #  adjust = 0.5,
  # move to the right
  #  justification = -0.2,
  # remove the slub interval
  #  .width = 0,
  #  point_colour = NA
  #) +
  
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.5
  ) +
  stat_dots(
    # ploting on left side
    side = "right",
    # adjusting position
    justification = 1.2,
    # adjust grouping (binning) of observations
    #binwidth = 0.25
    alpha = 0.2
  ) +
  
  # Themes and Labels
  scale_fill_tq() +
  theme_tq() +
  labs(
    title = "RainCloud Plot",
    x = "Sample Size",
    y = "PMP1u",
    fill = "test"
  ) +
  facet_wrap(~ n_sample, scales = "free") +
  theme(
    axis.ticks.y = element_blank(), 
    axis.text.y = element_blank()    
  )