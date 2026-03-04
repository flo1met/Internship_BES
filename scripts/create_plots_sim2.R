# create and save plots
library(tidyverse)
library(ggdist)

# Consistent Bayesian colour palette (colorblind-friendly)
col_primary   <- "#8B1A1A"   # dark red / maroon
col_secondary <- "#2166AC"   # steel blue
col_box_fill  <- "#EACBCB"   # light rose

# Create plots directory if it doesn't exist
if (!dir.exists("plots/Sim2/")) {
  dir.create("plots/Sim2/", recursive = TRUE)
}

load("data/Sim_2.RData")
df_with_na <- Sim_2[apply(is.na(Sim_2), 1, any), ]
cumprod(c(0,0.5,1)) / (cumprod(c(0,0.5,1)) + cumprod(c(1-0.000001,1-0.5,1-0.9999999)))
#therefore
Sim_2$BESc <- ifelse(is.nan(Sim_2$BESc), 1, Sim_2$BESc)

Sim_2$n_sample <- Sim_2$n_sample*3

# Add indicator or partial
Sim_2$ind_p_all <- ifelse(Sim_2$ind_p1 == 1 & Sim_2$ind_p2 == 1 & Sim_2$ind_p3 == 1, 1, 0)

Sim_2_agg <- Sim_2 %>%
  group_by(n_sample, c, d) %>%
  summarise(PMP1u_H = mean(PMP1u_H),
            PMP1c_H = mean(PMP1c_H),
            BESc = mean(BESc),
            BESu = mean(BESu))


#############################################################################
## Lineplot: Bayes Factor tested against the unconstrained Hypothesis ##
#############################################################################


lineplot_unconstrained <- Sim_2_agg %>% ggplot() +
  geom_line(aes(
    x = as.factor(n_sample),
    y = PMP1u_H,
    linetype = "Complete",
    color = "Complete",
    group = 1
  )) +
  geom_line(aes(
    x = as.factor(n_sample),
    y = BESu,
    linetype = "BES",
    color = "BES",
    group = 1
  )) +
  scale_linetype_manual(name = "Method",
                        values = c("Complete" = "solid", "BES" = "longdash")) +
  scale_color_manual(name = "Method",
                     values = c("Complete" = col_primary, "BES" = col_secondary)) +
  geom_hline(yintercept = 8/(8+1), color = "grey40", linetype = "dotted", alpha = 0.5) +
  geom_hline(yintercept = 24/(24+1), color = "grey70", linetype = "dotted", alpha = 0.5) +
  labs(
    x = "Sample Size",
    y = "Posterior Model Probabilities"
  ) +
  theme_minimal(base_size = 11) +
  facet_grid(c ~ d) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

ggsave(paste0("plots/Sim2/s2_lineplot_unconstrained",".pdf"), lineplot_unconstrained, 
       width = 21, height = 29.7, units = "cm", dpi = 300)


#####################################################################
## Lineplot: Bayes Factor tested against the complement Hypothesis ##
#####################################################################

lineplot_complement <- Sim_2_agg %>% ggplot() +
  geom_line(aes(
    x = as.factor(n_sample),
    y = PMP1c_H,
    linetype = "Complete",
    color = "Complete",
    group = 1
  )) +
  geom_line(aes(
    x = as.factor(n_sample),
    y = BESc,
    linetype = "BES",
    color = "BES",
    group = 1
  )) +
  scale_linetype_manual(name = "Method",
                        values = c("Complete" = "solid", "BES" = "longdash")) +
  scale_color_manual(name = "Method",
                     values = c("Complete" = col_primary, "BES" = col_secondary)) +
  labs(
    x = "Sample Size",
    y = "Posterior Model Probabilities"
  ) +
  theme_minimal(base_size = 11) +
  facet_grid(c ~ d) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

ggsave(paste0("plots/Sim2/s2_lineplot_complement",".pdf"), lineplot_complement, 
       width = 21, height = 29.7, units = "cm", dpi = 300)

########################
## Distplot all cases ##
########################

# Define parameters
sample_sizes <- c(75, 150, 300, 1500)
correlations <- c(0, 0.2, 0.5, 0.8)
effect_sizes <- c(0, 0.2, 0.5, 0.8)

# Define plot configurations
plot_configs <- list(
  list(
    var = "BESc",
    fill_var = "ind_comp",
    title = "Bayesian Evidence Synthesis against Complement",
    x_label = "Posterior Model Probabilities against Complement",
    name = "BESc_complement"
  ),
  list(
    var = "PMP1c_H",
    fill_var = "ind_p_all",
    title = "PMPs against Complement",
    x_label = "Posterior Model Probabilities",
    name = "PMP1c_complement"
  ),
  list(
    var = "BESu",
    fill_var = "ind_comp",
    title = "Bayesian Evidence Synthesis against Unconstrained",
    x_label = "Posterior Model Probabilities",
    name = "BESu_unconstrained"
  ),
  list(
    var = "PMP1u_H",
    fill_var = "ind_p_all",
    title = "PMPs against Unconstrained",
    x_label = "Posterior Model Probabilities",
    name = "PMP1u_unconstrained"
  )
)

# Iterate through all combinations
for (n in sample_sizes) {
  for (corr in correlations) {
    for (eff in effect_sizes) {
      
      # Filter data for current combination
      filtered_data <- Sim_2 %>% 
        filter(n_sample == n & c == corr & d == eff)
      
      # Skip if no data for this combination
      if (nrow(filtered_data) == 0) {
        next
      }
      
      # Create each plot type
      for (config in plot_configs) {
        
        p <- filtered_data %>%
          ggplot(aes(x = .data[[config$var]])) +
          
          stat_dots(
            aes(y = 0),
            fill = col_primary,
            color = col_primary,
            side = "top",
            scale = 0.8,
            alpha = 0.6,
            dotsize = 1.5
          ) +
          
          geom_boxplot(
            aes(y = -0.15),
            fill = col_box_fill,
            color = col_primary,
            width = 0.05,
            outlier.shape = 16,
            outlier.size = 1,
            outlier.alpha = 0.1,
            alpha = 0.7
          ) +
          
          theme_minimal(base_size = 11) +
          labs(
            title = paste0(config$title,
                           "\nn = ", n, ", c = ", corr, ", d = ", eff),
            x = config$x_label,
            y = ""
          ) +
          theme(
            strip.text = element_text(size = 11, face = "bold"),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(hjust = 0.5)
          )
        
        # Create filename
        filename <- paste0("plots/Sim2/", config$name, 
                           "_n", n, "_c", corr, "_d", eff, ".pdf")
        
        # Save plot
        ggsave(filename, plot = p,
               width = 7, height = 4.5, 
               units = "in", 
               device = "pdf")
        
        cat("Saved:", filename, "\n")
      }
    }
  }
}
