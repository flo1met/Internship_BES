# create and save plots
library(tidyverse)
library(tidyquant)
library(ggdist)

# Create plots directory if it doesn't exist
if (!dir.exists("plots/Sim4/")) {y
  dir.create("plots/Sim4/", recursive = TRUE)
}

load("data/Sim_4.RData")
df_with_na <- Sim_4[apply(is.na(Sim_4), 1, any), ]
# "impute" all NaN with 1 because:
cumprod(c(1,0,1)) / (cumprod(c(1,0,1)) + cumprod(c(1-1,1-0,1-1)))

cumprod(c(1,0.0000001,1)) / (cumprod(c(1,0.0000001,1)) + cumprod(c(1-1,1-0.0000001,1-1)))

# correct?
Sim_4$BESc <- ifelse(is.nan(Sim_4$BESc), 1, Sim_4$BESc)
Sim_4$n_sample <- Sim_4$n_sample*3

# Add indicator or partial
Sim_4$ind_p_all <- ifelse(Sim_4$ind_p1 == 1 & Sim_4$ind_p2 == 1 & Sim_4$ind_p3 == 1, 1, 0)

Sim_4_agg <- Sim_4 %>%
  group_by(n_sample, c, d) %>%
  summarise(PMP1u_H = mean(PMP1u_H),
            PMP1c_H = mean(PMP1c_H),
            BESc = mean(BESc),
            BESu = mean(BESu))

########################################################################
## Lineplot: Bayes Factor tested against the unconstrained Hypothesis ##
########################################################################

lineplot_unconstrained <- Sim_4_agg %>% ggplot() +
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
  scale_linetype_manual(name = "Hypothesis",
                        values = c("Complete" = "solid", "BES" = "longdash")) +
  scale_color_manual(name = "Hypothesis",
                     values = c("Complete" = "#00BFC4", "BES" = "#F8766D")) +
  ggtitle("Comparison PMPs of BES and complete Hypothesis against the Complement") +
  theme_bw() +
  facet_grid(c ~ d) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0("plots/Sim4/s4_lineplot_unconstrained",".pdf"), lineplot_unconstrained, 
       width = 21, height = 29.7, units = "cm", dpi = 300)

#####################################################################
## Lineplot: Bayes Factor tested against the complement Hypothesis ##
#####################################################################

lineplot_complement <- Sim_4_agg %>% ggplot() +
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
  scale_linetype_manual(name = "Hypothesis",
                        values = c("Complete" = "solid", "BES" = "longdash")) +
  scale_color_manual(name = "Hypothesis",
                     values = c("Complete" = "#00BFC4", "BES" = "#F8766D")) +
  geom_hline(yintercept = 8/(8+1), color = "black", linetype = "dotted", alpha = 0.5) +
  geom_hline(yintercept = 24/(24+1), color = "grey", linetype = "dotted", alpha = 0.5) +
  labs(x = "Sample Size", y = "Posterior Model Probabilities") +
  ggtitle("Comparison PMPs of BES and complete Hypothesis against the Unconstrained") +
  theme_bw() +
  facet_grid(c ~ d) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0("plots/Sim4/s4_lineplot_complement",".pdf"), lineplot_complement, 
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
      filtered_data <- Sim_4 %>% 
        filter(n_sample == n & c == corr & d == eff)
      
      # Skip if no data for this combination
      if (nrow(filtered_data) == 0) {
        next
      }
      
      # Create each plot type
      for (config in plot_configs) {
        
        p <- filtered_data %>%
          ggplot(aes(x = .data[[config$var]], 
                     fill = factor(.data[[config$fill_var]]), 
                     color = factor(.data[[config$fill_var]]))) +
          
          stat_dots(
            aes(y = 0),
            side = "top",
            scale = 0.8,
            alpha = 0.6,
            dotsize = 1.5
          ) +
          
          geom_boxplot(
            aes(y = -0.15),
            width = 0.05,
            outlier.shape = 16,
            outlier.size = 1,
            alpha = 0.7
          ) +
          
          scale_fill_tq() +
          scale_color_tq() +
          theme_tq() +
          labs(
            title = paste0(config$title, 
                           "\nn = ", n, ", c = ", corr, ", d = ", eff),
            x = config$x_label,
            y = "",
            fill = "Hypothesis Indicator",
            color = "Hypothesis Indicator"
          ) +
          theme(
            strip.text = element_text(size = 11, face = "bold"),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(hjust = 0.5)
          )
        
        # Create filename
        filename <- paste0("plots/Sim4/", config$name, 
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

cat("\nAll plots saved successfully!\n")